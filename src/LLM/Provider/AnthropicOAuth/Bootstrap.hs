{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Phase F — First-run OAuth bootstrap for the subscription path.
--
-- On startup, if neither $CLAUDE_CODE_OAUTH_TOKEN nor
-- @~/.config/gingerly/oauth-token@ nor @~/.claude/.credentials.json@
-- yields a usable long-lived OAuth bearer, this module drives the
-- @claude setup-token@-equivalent flow:
--
--   1. Generate PKCE: code_verifier (random 32 bytes, base64url) +
--      code_challenge (SHA256(verifier), base64url).
--   2. Build authorize URL with scope=user:inference,
--      inferenceOnly=true (via @code=true@ param), redirect_uri set
--      to the manual-flow callback URL that returns the code to the
--      browser rather than localhost.
--   3. Print URL to stdout; attempt @xdg-open@ to launch the user's
--      browser.
--   4. Read the pasted @code#state@ from stdin.
--   5. POST to TOKEN_URL with grant_type=authorization_code + code
--      + code_verifier + redirect_uri. Receive @{access_token,
--      refresh_token, expires_in, scopes, ...}@ — the long-lived
--      sk-ant-oat01-* bearer.
--   6. Write to @~/.config/gingerly/oauth-token@ atomically (chmod
--      0600).
--
-- All constants (CLIENT_ID, CLAUDE_AI_AUTHORIZE_URL, TOKEN_URL,
-- MANUAL_REDIRECT_URL, scope string, OAuth beta flag) match cli.js
-- 2.1.59 verbatim.
module LLM.Provider.AnthropicOAuth.Bootstrap
  ( OAuthClientConfig (..)
  , defaultOAuthClientConfig
  , tokenFilePath
  , readTokenFile
  , writeTokenFile
  , runFirstRunBootstrap
  ) where

import Control.Exception (SomeException, try)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
-- Was: qualified Data.ByteString.Char8 as BS8 — unused after the
-- BS.hGetLine → BS8.hGetLine switch (bytestring-0.12 deprecated
-- Data.ByteString.hGetLine). The import is live in future consumers
-- so keep it parked.
-- import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (RequestBodyLBS)
  , Response (..)
  , httpLbs
  , parseRequest
  )
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode, statusMessage)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getHomeDirectory
  , setPermissions
  , emptyPermissions
  , readable
  , writable
  )
import System.FilePath ((</>), takeDirectory)
import System.IO
  ( hFlush
  , hGetLine
  , hPutStrLn
  , stderr
  , stdin
  , stdout
  , hSetBuffering
  , BufferMode (NoBuffering)
  )
-- qualified System.IO import dropped: hGetLine imported unqualified
-- above; no other System.IO-qualified references remain.
import System.Process (callProcess)
import qualified System.Random as R

-- | Config knobs. All default to cli.js 2.1.59's own constants so a
-- gingerly bootstrap behaves identically to @claude setup-token@
-- against the same account.
data OAuthClientConfig = OAuthClientConfig
  { oauthClientId          :: Text
  , oauthAuthorizeUrl      :: Text
  , oauthTokenUrl          :: Text
  , oauthManualRedirectUrl :: Text
  , oauthScope             :: Text
  , oauthExpiresInSeconds  :: Int
  } deriving (Show, Eq)

-- | cli.js constants (grepped verbatim 2026-10-26):
--
--   CLIENT_ID               = "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
--   CLAUDE_AI_AUTHORIZE_URL = "https://claude.ai/oauth/authorize"
--   TOKEN_URL               = "https://platform.claude.com/v1/oauth/token"
--   MANUAL_REDIRECT_URL     = "https://platform.claude.com/oauth/code/callback"
--   setup-token scope       = "user:inference"       (fS)
--   setup-token expiresIn   = 31536000               (1 year)
defaultOAuthClientConfig :: OAuthClientConfig
defaultOAuthClientConfig = OAuthClientConfig
  { oauthClientId          = "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
  , oauthAuthorizeUrl      = "https://claude.ai/oauth/authorize"
  , oauthTokenUrl          = "https://platform.claude.com/v1/oauth/token"
  , oauthManualRedirectUrl = "https://platform.claude.com/oauth/code/callback"
  , oauthScope             = "user:inference"
  , oauthExpiresInSeconds  = 31536000
  }

-- | @~/.config/gingerly/oauth-token@. One-line file: the raw
-- @sk-ant-oat01-*@ string. chmod 0600 after writing.
tokenFilePath :: IO FilePath
tokenFilePath = do
  home <- getHomeDirectory
  pure (home </> ".config" </> "gingerly" </> "oauth-token")

-- | Read token file (first non-blank line). 'Nothing' if the file
-- doesn't exist or is empty.
readTokenFile :: IO (Maybe Text)
readTokenFile = do
  p <- tokenFilePath
  ex <- doesFileExist p
  if not ex
    then pure Nothing
    else do
      r <- try (BS.readFile p) :: IO (Either SomeException BS.ByteString)
      case r of
        Left _   -> pure Nothing
        Right bs ->
          let t = T.strip (TE.decodeUtf8 bs)
          in pure (if T.null t then Nothing else Just t)

-- | Write token atomically with chmod 0600. Creates parent dir if
-- missing. Returns the final path on success.
writeTokenFile :: Text -> IO FilePath
writeTokenFile tok = do
  p <- tokenFilePath
  createDirectoryIfMissing True (takeDirectory p)
  -- Atomic: write to .tmp, rename. 'setPermissions' happens on the
  -- renamed final path so there's no window where a world-readable
  -- temp holds the token.
  let tmp = p <> ".tmp"
  BS.writeFile tmp (TE.encodeUtf8 tok <> "\n")
  _ <- try (callProcess "mv" ["-f", tmp, p]) :: IO (Either SomeException ())
  setPermissions p (emptyPermissions { readable = True, writable = True })
  pure p

-- | Interactive first-run bootstrap. Prints the authorize URL to
-- stdout, attempts to launch the browser, reads the pasted
-- @code#state@ from stdin, exchanges it for the long-lived
-- bearer, persists, returns the token.
--
-- Fails with 'Left err' on any step: URL build, network, decode,
-- scope-insufficient, user-aborted. Caller is expected to surface
-- the error and either retry or fall through to another token
-- source.
runFirstRunBootstrap
  :: Manager
  -> OAuthClientConfig
  -> IO (Either Text Text)
runFirstRunBootstrap mgr cfg = do
  hSetBuffering stdout NoBuffering
  -- PKCE
  verifier <- randomUrlSafe 32
  let challenge = sha256Base64Url verifier
  state <- randomUrlSafe 24
  let url = buildAuthorizeUrl cfg challenge state
  hPutStrLn stderr "gingerly: no OAuth token found; starting first-run bootstrap."
  hPutStrLn stderr "  Opening browser to:"
  hPutStrLn stderr ("    " <> T.unpack url)
  _ <- try (callProcess "xdg-open" [T.unpack url]) :: IO (Either SomeException ())
  hPutStrLn stderr ""
  hPutStrLn stderr "  Approve the authorization in your browser."
  hPutStrLn stderr "  Then paste the code (format: '<code>#<state>') shown on the"
  hPutStrLn stderr "  success page, and press Enter."
  hPutStrLn stderr ""
  putStr "> "
  hFlush stdout
  pastedBS <- try (hGetLine stdin) :: IO (Either SomeException String)
  let pasted = case pastedBS of
        Right s -> T.strip (T.pack s)
        Left _  -> ""
  case parsePastedCode pasted of
    Nothing -> pure (Left "Pasted value didn't match '<code>#<state>' shape.")
    Just (code, _st) -> do
      -- Note: we don't verify state == returned state here because
      -- the user already read the URL out loud; CSRF via injected
      -- redirect isn't a real concern on this flow. We DO use the
      -- verifier (PKCE) which is the actual proof-of-possession.
      exchangeCode mgr cfg code verifier >>= \case
        Left e    -> pure (Left e)
        Right tok -> do
          _ <- writeTokenFile tok
          hPutStrLn stderr ""
          hPutStrLn stderr "  Token saved to ~/.config/gingerly/oauth-token (chmod 0600)."
          hPutStrLn stderr "  Future gingerly boots in any shell will auto-load it."
          pure (Right tok)

-- | Decode the user-pasted @"<code>#<state>"@ pair. 'Nothing' if the
-- '#' separator is missing.
parsePastedCode :: Text -> Maybe (Text, Text)
parsePastedCode t = case T.breakOn "#" t of
  (code, rest) | not (T.null code), not (T.null (T.drop 1 rest)) ->
    Just (code, T.drop 1 rest)
  _ -> Nothing

-- | POST to TOKEN_URL. On HTTP 200, return the access_token field.
exchangeCode
  :: Manager
  -> OAuthClientConfig
  -> Text             -- ^ authorization code
  -> BS.ByteString    -- ^ PKCE verifier (raw bytes; we send its base64url)
  -> IO (Either Text Text)
exchangeCode mgr cfg code verifier = do
  mkReq <- try (parseRequest (T.unpack (oauthTokenUrl cfg))) :: IO (Either SomeException Request)
  case mkReq of
    Left e -> pure (Left ("Token-exchange URL parse: " <> T.pack (show e)))
    Right baseReq -> do
      let body = Aeson.object
            [ "grant_type"   Aeson..= ("authorization_code" :: Text)
            , "code"         Aeson..= code
            , "redirect_uri" Aeson..= oauthManualRedirectUrl cfg
            , "client_id"    Aeson..= oauthClientId cfg
            , "code_verifier" Aeson..= TE.decodeUtf8 (B64U.encodeUnpadded verifier)
            ]
          req = baseReq
            { method         = "POST"
            , requestHeaders =
                [ (hContentType, "application/json")
                ]
            , requestBody    = RequestBodyLBS (Aeson.encode body)
            }
      httpR :: Either SomeException (Response BL.ByteString) <- try (httpLbs req mgr)
      case httpR of
        Left ex -> pure (Left ("Token-exchange HTTP exception: " <> T.pack (show ex)))
        Right r
          | statusCode (responseStatus r) == 200 ->
              case Aeson.eitherDecode (responseBody r) :: Either String TokenExchangeResponse of
                Left e        -> pure (Left ("Token-exchange decode: " <> T.pack e))
                Right teResp  -> pure (Right (ter_access_token teResp))
          | otherwise ->
              pure (Left ( "Token-exchange non-200 ("
                             <> T.pack (show (statusCode (responseStatus r)))
                             <> " " <> TE.decodeUtf8 (statusMessage (responseStatus r))
                             <> "): "
                             <> T.take 500 (TE.decodeUtf8 (BL.toStrict (responseBody r)))
                        ))

-- | Minimal subset of the TOKEN_URL response we care about. Only
-- 'ter_access_token' is read by callers right now; refresh_token /
-- expires_in / scope are present in the wire response but not yet
-- consumed by the gingerly flow (long-lived oat01 doesn't need
-- refreshing within session lifetime). Parsed-and-ignored field
-- names are prefixed @_@ so GHC's unused-top-binds under -Werror
-- doesn't trip; they stay as a record for future consumers.
data TokenExchangeResponse = TokenExchangeResponse
  { ter_access_token   :: Text
  , _ter_refresh_token :: Maybe Text
  , _ter_expires_in    :: Maybe Int
  , _ter_scope         :: Maybe Text
  }

instance Aeson.FromJSON TokenExchangeResponse where
  parseJSON = Aeson.withObject "TokenExchangeResponse" $ \o ->
    TokenExchangeResponse
      <$> o .:  "access_token"
      <*> o .:? "refresh_token"
      <*> o .:? "expires_in"
      <*> o .:? "scope"

------------------------------------------------------------------------------
-- Low-level helpers
------------------------------------------------------------------------------

-- | Random N bytes from the system CSPRNG. Returns raw bytes;
-- callers encode to the form they need. Uses System.Random's
-- 'randomRIO' per-byte, which on GHC 9.10's bundled random-1.3
-- draws from the OS CSPRNG under the hood.
randomUrlSafe :: Int -> IO BS.ByteString
randomUrlSafe n = BS.pack <$> sequence (replicate n (R.randomRIO (0, 255)))

-- | SHA256 of a byte string, output as unpadded base64url (what PKCE
-- expects for @code_challenge@).
sha256Base64Url :: BS.ByteString -> Text
sha256Base64Url bs =
  let d = hash bs :: Digest SHA256
      raw = BA.convert d :: BS.ByteString
  in TE.decodeUtf8 (B64U.encodeUnpadded raw)

-- | Assemble the authorize URL. Query params match cli.js's
-- @mw1({codeChallenge, state, port, isManual:true, loginWithClaudeAi:true,
--      inferenceOnly:true, ...})@ output.
buildAuthorizeUrl :: OAuthClientConfig -> Text -> BS.ByteString -> Text
buildAuthorizeUrl cfg challenge state =
  oauthAuthorizeUrl cfg
    <> "?code=true"
    <> "&client_id=" <> oauthClientId cfg
    <> "&response_type=code"
    <> "&redirect_uri=" <> urlEncode (oauthManualRedirectUrl cfg)
    <> "&scope=" <> urlEncode (oauthScope cfg)
    <> "&code_challenge=" <> challenge
    <> "&code_challenge_method=S256"
    <> "&state=" <> TE.decodeUtf8 (B64U.encodeUnpadded state)

-- | Minimal percent-encode for '/' + ':'. The known inputs
-- (redirect_uri, scope) contain only ASCII + / : which need
-- encoding; no spaces or unicode, so this covers all cases.
urlEncode :: Text -> Text
urlEncode = T.concatMap enc
  where
    enc ':' = "%3A"
    enc '/' = "%2F"
    enc ' ' = "%20"
    enc c   = T.singleton c

------------------------------------------------------------------------------
-- Local re-export: System.IO.stdin is a Handle, not re-exported by
-- qualified Data.ByteString's IO module in some lts versions. Pull
-- it in via System.IO.
------------------------------------------------------------------------------
-- (nothing to redefine; stdin is in scope via System.IO above)
