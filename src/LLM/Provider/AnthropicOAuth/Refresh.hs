{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: OAuth2 @refresh_token@ grant for Anthropic Claude Code
-- subscription credentials.
-- Copyright: (c) lazyLambda, 2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
--
-- Claude Code is a public OAuth2 client (no client_secret). Its refresh
-- flow is:
--
-- @
--   POST https:\/\/console.anthropic.com\/v1\/oauth\/token
--   Content-Type: application\/json
--   {
--     "grant_type":    "refresh_token",
--     "refresh_token": "sk-ant-ort-...",
--     "client_id":     "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
--   }
-- @
--
-- Response:
--
-- @
--   {
--     "access_token":  "sk-ant-oat-...",
--     "refresh_token": "sk-ant-ort-...",
--     "expires_in":    3600,
--     "scope":         "user:inference user:profile ...",
--     "token_type":    "Bearer"
--   }
-- @
--
-- These values were extracted verbatim from Claude Code's 'cli.js' at
-- version 1.0.21 on 2026-10-25. Anthropic does not publish this
-- endpoint in official docs; it's reverse-engineered from the official
-- CLI and matches the public OAuth2 refresh grant (RFC 6749 §6).
--
-- The 'RefreshError' sum surfaces each failure mode explicitly — no
-- silent 'Maybe', per CLAUDE.md "Crash, don't fail silently" and
-- "Bubble up errors — never bury them in the wrapper".
module LLM.Provider.AnthropicOAuth.Refresh
  ( -- * Constants
    anthropicClientId
  , anthropicTokenUrl
    -- * Refresh flow
  , TokenResponse (..)
  , refreshAccessToken
    -- * Failure modes
  , RefreshError (..)
    -- * Credentials helper
  , applyRefresh
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
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
import qualified Data.Text.Encoding as TE

import LLM.Provider.AnthropicOAuth.Credentials (Credentials (..))

-- | Public OAuth2 @client_id@ for the Claude Code CLI. Extracted from
-- @cli.js@ 1.0.21 at @t8().CLIENT_ID@. Public clients embed their
-- client_id in distributed binaries; there is no @client_secret@.
anthropicClientId :: Text
anthropicClientId = "9d1c250a-e61b-44d9-88ed-5944d1962f5e"

-- | OAuth2 token endpoint for the subscription flow. Note: this is
-- @console.anthropic.com@ (web auth surface), NOT @api.anthropic.com@
-- (inference surface). The access token issued here is spent against
-- the latter.
anthropicTokenUrl :: Text
anthropicTokenUrl = "https://console.anthropic.com/v1/oauth/token"

-- | Decoded body of a successful token refresh response.
data TokenResponse = TokenResponse
  { trAccessToken  :: Text
  , trRefreshToken :: Text
    -- ^ Anthropic rotates refresh tokens on every refresh (per OAuth2
    -- security best practice). The new one MUST replace the old on
    -- disk — the old one is single-use.
  , trExpiresIn    :: Int
    -- ^ Seconds-from-now the access token is valid. Typically 3600
    -- (1h) on Max subscriptions; the file persists an absolute
    -- 'expiresAt' millis-since-epoch computed at refresh time.
  , trScope        :: Text
    -- ^ Space-separated scope list. Converted to '[Text]' when
    -- merged back into 'Credentials'.
  , trTokenType    :: Text
    -- ^ Always @"Bearer"@. Validated loosely (case-insensitive) when
    -- used; not re-sent to the server.
  } deriving (Show, Eq, Generic)

instance FromJSON TokenResponse where
  parseJSON = Aeson.withObject "oauth-token-response" $ \o -> TokenResponse
    <$> o .: "access_token"
    <*> o .: "refresh_token"
    <*> o .: "expires_in"
    <*> o .: "scope"
    <*> o .: "token_type"

-- | RFC 6749 §5.2 error response: @{error, error_description}@. Used
-- by Anthropic for @invalid_grant@ (refresh_token revoked/expired),
-- @invalid_client@, @unauthorized_client@, etc. @error_description@
-- is optional per the spec.
data OAuthErrorBody = OAuthErrorBody
  { oauthError            :: Text
  , oauthErrorDescription :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON OAuthErrorBody where
  parseJSON = Aeson.withObject "oauth-error" $ \o -> OAuthErrorBody
    <$> o .:  "error"
    <*> o .:? "error_description"

-- | Failure modes of a refresh attempt. Each case is distinct because
-- the right recovery differs: HTTP errors suggest retry-with-backoff,
-- decode errors suggest upstream schema drift, OAuth errors are
-- authoritative "this refresh_token is no good, user must re-login".
data RefreshError
  = RefreshHttpException Text
    -- ^ Network / TLS / timeout failure. Wrapped exception as text.
  | RefreshNonSuccessStatus Int Text
    -- ^ Non-2xx response that was NOT a parseable OAuth error body.
    -- Fields: status code, status message. Response body is
    -- intentionally omitted to avoid logging sensitive data.
  | RefreshDecodeError Text
    -- ^ 2xx response body didn't match 'TokenResponse' shape (schema
    -- bump upstream?). Wrapped aeson error.
  | RefreshOAuthError Text (Maybe Text)
    -- ^ RFC 6749 @{error, error_description}@ response. Code strings
    -- like @"invalid_grant"@ mean "refresh_token revoked, user must
    -- re-authenticate" — the caller should surface that to the user.
  deriving (Show, Eq)

-- | Attempt to exchange a 'credRefreshToken' for a fresh access
-- token. Does NOT persist the result — the caller is responsible for
-- calling 'applyRefresh' + 'atomicWriteCredentials' to update the
-- file. Separation lets tests exercise the refresh path without
-- touching the real file.
refreshAccessToken :: Manager -> Text -> IO (Either RefreshError TokenResponse)
refreshAccessToken mgr refreshToken = do
  mkReq <- try (parseRequest (T.unpack anthropicTokenUrl)) :: IO (Either SomeException Request)
  case mkReq of
    Left e -> pure (Left (RefreshHttpException (T.pack (show e))))
    Right baseReq -> do
      let body = Aeson.encode $ Aeson.object
            [ "grant_type"    .= ("refresh_token" :: Text)
            , "refresh_token" .= refreshToken
            , "client_id"     .= anthropicClientId
            ]
          req = baseReq
            { method = "POST"
            , requestHeaders = [(hContentType, "application/json")]
            , requestBody    = RequestBodyLBS body
            }
      httpResult :: Either SomeException (Response BL.ByteString) <- try (httpLbs req mgr)
      case httpResult of
        Left e  -> pure (Left (RefreshHttpException (T.pack (show e))))
        Right r -> pure (classifyRefreshResponse r)

classifyRefreshResponse :: Response BL.ByteString -> Either RefreshError TokenResponse
classifyRefreshResponse r =
  let code = statusCode (responseStatus r)
      msg  = TE.decodeUtf8 (statusMessage (responseStatus r))
      bs   = responseBody r
  in if code >= 200 && code < 300
     then case Aeson.eitherDecode bs of
            Right tr -> Right tr
            Left e   -> Left (RefreshDecodeError (T.pack e))
     else case Aeson.eitherDecode bs :: Either String OAuthErrorBody of
            Right oe -> Left (RefreshOAuthError (oauthError oe) (oauthErrorDescription oe))
            Left _   -> Left (RefreshNonSuccessStatus code msg)

-- | Fold a successful 'TokenResponse' into an existing 'Credentials'
-- record, producing the shape to persist back to disk. Preserves the
-- 'credSubscriptionType' and 'credRateLimitTier' from the prior
-- credentials — the refresh response doesn't carry them.
applyRefresh :: UTCTime -> Credentials -> TokenResponse -> Credentials
applyRefresh now prior tr = prior
  { credAccessToken  = trAccessToken tr
  , credRefreshToken = trRefreshToken tr
  , credExpiresAt    = nowMillis + fromIntegral (trExpiresIn tr) * 1000
  , credScopes       = T.words (trScope tr)
  }
  where
    nowMillis :: Int64
    nowMillis = round (utcTimeToPOSIXSeconds now * 1000)
