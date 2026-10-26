{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Description: Read + write Anthropic Claude Code's subscription-OAuth
-- credentials file (@~/.claude/.credentials.json@).
-- Copyright: (c) lazyLambda, 2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
--
-- Claude Code's @~/.claude/.credentials.json@ carries an OAuth2 bearer
-- token under the @claudeAiOauth@ nested object. The 'Credentials'
-- record below mirrors the keys verified by @jq@ against a real file
-- on 2026-10-25: @accessToken@, @refreshToken@, @expiresAt@ (JS-style
-- millis-since-epoch), @scopes@, @subscriptionType@, @rateLimitTier@.
--
-- The library NEVER hardcodes the credentials path — call
-- 'defaultCredentialsPath' at runtime (per CLAUDE.md
-- 'Never hardcode local file paths'); consumers that want a different
-- location (test, non-standard user profile) pass an explicit
-- 'FilePath' to 'loadCredentials' \/ 'atomicWriteCredentials'.
module LLM.Provider.AnthropicOAuth.Credentials
  ( -- * Credentials record
    Credentials (..)
    -- * File IO
  , defaultCredentialsPath
  , loadCredentials
  , atomicWriteCredentials
    -- * Failure modes
  , LoadError (..)
    -- * UTC-time helpers
  , expiresAtUTC
  , hasExpired
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getHomeDirectory, renameFile)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (hClose)
import System.IO.Temp (openBinaryTempFile)

-- | The @claudeAiOauth@ sub-object of @~/.claude/.credentials.json@.
--
-- Field names mirror the on-disk JSON (camelCase, not Haskell underscore
-- convention) because the file is co-owned with Claude Code — if we
-- renamed the fields in our ToJSON, every write would corrupt Claude
-- Code's view of the file.
data Credentials = Credentials
  { credAccessToken     :: Text
    -- ^ Bearer token; sent as @Authorization: Bearer \<accessToken\>@.
  , credRefreshToken    :: Text
    -- ^ Used in @grant_type=refresh_token@ POST when 'credAccessToken'
    -- is near expiry or rejected by the server.
  , credExpiresAt       :: Int64
    -- ^ @Date.now()@-style millis-since-epoch, computed by Claude Code
    -- as @Date.now() + expires_in*1000@ when the token was minted.
    -- Stored as 'Int64' (not 'UTCTime') to roundtrip bit-exact with the
    -- on-disk file; use 'expiresAtUTC' when reasoning about freshness.
  , credScopes          :: [Text]
    -- ^ e.g. @["user:inference", "user:profile", ...]@. The library
    -- doesn't inspect these — Anthropic's server enforces scope checks.
  , credSubscriptionType :: Text
    -- ^ Observed values: @"max"@ (Max subscription). Opaque to the
    -- library; stored for diagnostics.
  , credRateLimitTier    :: Text
    -- ^ Observed values: @"default_claude_max_20x"@. Opaque; diagnostics.
  } deriving (Show, Eq, Generic)

instance FromJSON Credentials where
  parseJSON = Aeson.withObject "credentials.json" $ \o -> do
    oauth <- o .: "claudeAiOauth"
    Aeson.withObject "claudeAiOauth" (\a -> Credentials
      <$> a .: "accessToken"
      <*> a .: "refreshToken"
      <*> a .: "expiresAt"
      <*> a .: "scopes"
      <*> a .: "subscriptionType"
      <*> a .: "rateLimitTier") oauth

instance ToJSON Credentials where
  toJSON c = Aeson.object
    [ "claudeAiOauth" .= Aeson.object
        [ "accessToken"      .= credAccessToken c
        , "refreshToken"     .= credRefreshToken c
        , "expiresAt"        .= credExpiresAt c
        , "scopes"           .= credScopes c
        , "subscriptionType" .= credSubscriptionType c
        , "rateLimitTier"    .= credRateLimitTier c
        ]
    ]

-- | 'credExpiresAt' as a Haskell 'UTCTime'. JS millis / 1000 → POSIX
-- seconds → UTCTime.
expiresAtUTC :: Credentials -> UTCTime
expiresAtUTC c = posixSecondsToUTCTime (fromIntegral (credExpiresAt c) / 1000)

-- | True iff the token has already expired as of @now@. Does NOT apply
-- a safety buffer — call site decides (typically "expired OR within
-- 60s of expiring" to avoid refreshing mid-request).
hasExpired :: UTCTime -> Credentials -> Bool
hasExpired now c = expiresAtUTC c <= now

-- | 'LoadError' is an explicit sum per CLAUDE.md "Crash, don't fail
-- silently — and treat every crash as an investigation" and "Every
-- struct constructor is a validator". No silent 'Maybe' here —
-- consumers get a precise reason for the failure and can decide to
-- bubble, log, re-prompt the user, or crash.
data LoadError
  = LoadErrorFileMissing FilePath
    -- ^ No file at the given path. User probably hasn't run
    -- @claude login@ on this machine; up to the caller to prompt.
  | LoadErrorIO FilePath Text
    -- ^ Open or read failed with an 'IOException' (permissions,
    -- truncated file, filesystem fault). The wrapped 'Text' is the
    -- exception's 'show' output.
  | LoadErrorDecode FilePath Text
    -- ^ The file exists but doesn't parse as the expected JSON shape.
    -- Could be a Claude Code schema bump (upstream added a field we
    -- don't know about — harmless, aeson tolerates extras) OR the
    -- file is corrupted. The wrapped 'Text' is aeson's error.
  deriving (Show, Eq)

-- | Default location: @$HOME\/.claude\/.credentials.json@.
--
-- Does NOT fall back to @$XDG_CONFIG_HOME@ because Claude Code itself
-- uses the fixed @~\/.claude\/@ directory — if the harness read from
-- a different path, it would diverge from the file @claude login@
-- actually writes to.
defaultCredentialsPath :: IO FilePath
defaultCredentialsPath = do
  home <- getHomeDirectory
  pure (home </> ".claude" </> ".credentials.json")

-- | Read + parse credentials. Explicit 'Either' per the
-- bubble-errors-up rule — the consumer decides whether a missing
-- file is a prompt-the-user case or a hard-fail.
loadCredentials :: FilePath -> IO (Either LoadError Credentials)
loadCredentials p = do
  exists <- doesFileExist p
  if not exists
    then pure (Left (LoadErrorFileMissing p))
    else try (BL.readFile p) >>= \case
      Left (ioe :: IOException) ->
        pure (Left (LoadErrorIO p (T.pack (show ioe))))
      Right bs -> case Aeson.eitherDecode bs of
        Left err  -> pure (Left (LoadErrorDecode p (T.pack err)))
        Right cs  -> pure (Right cs)

-- | Write credentials atomically. Writes to a tempfile in the same
-- directory, then @rename@s over the target — on POSIX the rename is
-- atomic, so a crash mid-write never corrupts the credentials file
-- (worst case a stray tempfile is left behind, which is harmless).
--
-- Co-tenancy: Claude Code may also be writing this file. Atomic rename
-- means our write doesn't observe a half-written Claude-Code write and
-- vice versa — the file is always either the old content or the new
-- content, never partial. (Does not protect against lost-update: if
-- Claude Code and the harness both refresh concurrently, one of their
-- new refresh_tokens wins and the other's is dropped. Acceptable per
-- the plan's "~24h token lifetime, refresh is cheap".)
atomicWriteCredentials :: FilePath -> Credentials -> IO ()
atomicWriteCredentials target c = do
  let dir      = takeDirectory target
      baseName = takeFileName target
  (tmpPath, h) <- openBinaryTempFile dir (baseName <> ".tmp")
  BL.hPut h (Aeson.encode c)
  hClose h
  renameFile tmpPath target

