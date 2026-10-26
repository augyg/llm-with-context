{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Description: OAuth-authenticated @POST \/v1\/messages@ against
-- @api.anthropic.com@, with on-401 refresh + atomic credentials rewrite.
-- Copyright: (c) lazyLambda, 2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
--
-- The counterpart to "LLM.Provider.Anthropic"'s servant-driven
-- @askClaude@ — same JSON body shape, same @\/v1\/messages@ endpoint,
-- different auth header. 'askAnthropicOAuth' uses raw @http-client@
-- (not servant) because the servant @MessagesAPI@ in
-- "LLM.Provider.Anthropic" hardcodes @Header "x-api-key"@ — forking
-- it just to swap one header name would be more code than a direct
-- POST.
--
-- Reuses 'AnthropicRequest' / 'AnthropicResponse' / 'splitSystem' /
-- 'firstText' from "LLM.Provider.Anthropic"; decodes the error-body
-- shape via 'AnthropicErrorResponse' / 'AnthropicAPIError'
-- so the body-building + response-parsing logic has one source of
-- truth.
module LLM.Provider.AnthropicOAuth.Ask
  ( -- * Environment
    AnthropicOAuthEnv (..)
  , newAnthropicOAuthEnv
    -- * Ask
  , askAnthropicOAuth
  , askAnthropicOAuthStream
    -- * Diagnostics
  , ensureFreshCredentials
    -- * Header constants
  , defaultAnthropicVersion
  , defaultAnthropicBeta
  , defaultUserAgent
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Network.HTTP.Client
  ( BodyReader
  , Manager
  , Request (..)
  , RequestBody (RequestBodyLBS)
  , Response (..)
  , brConsume
  , brRead
  , httpLbs
  , parseRequest
  , withResponse
  )
import Network.HTTP.Types.Header (hAuthorization, hContentType, hUserAgent)
import Network.HTTP.Types.Status (statusCode, statusMessage)

import LLM.Provider.Anthropic
  ( AnthropicAPIError (..)
  , AnthropicErrorResponse (..)
  , AnthropicRequest (..)
  , AnthropicResponse
  , firstText
  , splitSystem
  )
import LLM.Provider.AnthropicOAuth.Credentials
  ( Credentials (..)
  , LoadError (..)
  , atomicWriteCredentials
  , hasExpired
  , loadCredentials
  )
import LLM.Provider.AnthropicOAuth.Stream
  ( StreamEvent (..)
  , emptyStreamBuffer
  , feedBytes
  )
import LLM.Provider.AnthropicOAuth.Refresh
  ( RefreshError (..)
  , applyRefresh
  , refreshAccessToken
  )
import LLM.Types (ContentWithRole (..), GPTRole (..))

-- | Default @anthropic-version@ header value, matching Claude Code
-- 1.0.21's @cli.js@. Changing this means telling Anthropic's server
-- "I speak the 2023-06-01 wire protocol" — bump only when the server
-- deprecates it.
defaultAnthropicVersion :: Text
defaultAnthropicVersion = "2023-06-01"

-- | Default @anthropic-beta@ header value. Matches Claude Code 1.0.21's
-- advertisement of the @claude-code-20250219@ Claude-Code-flavored
-- inference beta.
defaultAnthropicBeta :: Text
defaultAnthropicBeta = "claude-code-20250219"

-- | Default @User-Agent@ header value. We advertise as @claude-cli@
-- (same prefix Claude Code 1.0.21 uses) to minimise anomaly-detection
-- risk per the plan ("match Claude Code's user-agent + request cadence
-- + request-shape where possible"). The suffix identifies the harness.
defaultUserAgent :: Text
defaultUserAgent = "claude-cli/llm-with-context-oauth"

-- | Runtime environment for the OAuth-authenticated backend. Shared
-- between calls via 'MVar' because refresh must be mutually exclusive
-- with reads of the same credential set (no torn 'accessToken'
-- writes).
--
-- The 'MVar' holds the latest in-memory copy of 'Credentials';
-- 'ensureFreshCredentials' consults it, refreshes if necessary, and
-- writes BOTH the file and the MVar atomically.
data AnthropicOAuthEnv = AnthropicOAuthEnv
  { envManager         :: Manager
  , envCredentialsPath :: FilePath
  , envCredentials     :: MVar Credentials
  , envModel           :: Text
  , envMaxTokens       :: Int
  , envAnthropicBaseUrl :: Text
  } deriving (Generic)

-- | Convenience constructor: 'loadCredentials' + build the MVar.
-- 'Left' iff the file is missing or malformed — the caller should
-- surface this to the user (e.g. "run `claude login` first").
newAnthropicOAuthEnv
  :: Manager
  -> FilePath   -- ^ credentials path (typically 'defaultCredentialsPath')
  -> Text       -- ^ model
  -> Int        -- ^ max_tokens
  -> Text       -- ^ base URL (typically @"https://api.anthropic.com"@)
  -> IO (Either LoadError AnthropicOAuthEnv)
newAnthropicOAuthEnv mgr credPath model maxToks baseUrl =
  loadCredentials credPath >>= \case
    Left e  -> pure (Left e)
    Right c -> do
      mv <- newMVar c
      pure $ Right AnthropicOAuthEnv
        { envManager          = mgr
        , envCredentialsPath  = credPath
        , envCredentials      = mv
        , envModel            = model
        , envMaxTokens        = maxToks
        , envAnthropicBaseUrl = baseUrl
        }

-- | If the current token is within 60s of expiring, refresh it and
-- persist to BOTH the file and the in-memory 'MVar'. Returns 'Left'
-- with a user-facing message if the refresh grant was rejected
-- (invalid_grant means the user must @claude login@ again) or if
-- network / decode fails.
--
-- Mutex: 'modifyMVar' serialises concurrent asks; the second ask
-- grabs the MVar after the first wrote the new token, re-checks
-- freshness via 'hasExpired', and finds nothing to do.
ensureFreshCredentials :: AnthropicOAuthEnv -> IO (Either Text ())
ensureFreshCredentials env = do
  now <- getCurrentTime
  let bufferedNow = addUTCTime 60 now
  modifyMVar (envCredentials env) $ \current ->
    if hasExpired bufferedNow current
      then do
        res <- refreshAccessToken (envManager env) (credRefreshToken current)
        case res of
          Left re -> pure (current, Left (renderRefreshError re))
          Right tr -> do
            let fresh = applyRefresh now current tr
            atomicWriteCredentials (envCredentialsPath env) fresh
            pure (fresh, Right ())
      else pure (current, Right ())

-- | Render a 'RefreshError' as human-readable text for diagnostics.
renderRefreshError :: RefreshError -> Text
renderRefreshError = \case
  RefreshHttpException t      -> "OAuth refresh HTTP exception: " <> t
  RefreshNonSuccessStatus c m -> "OAuth refresh non-success " <> T.pack (show c) <> " " <> m
  RefreshDecodeError t        -> "OAuth refresh response decode error: " <> t
  RefreshOAuthError code desc ->
    "OAuth error " <> code <> maybe "" (": " <>) desc
      <> if code == "invalid_grant"
         then " (refresh_token revoked — run `claude login` and retry)"
         else ""

-- | Perform a text ask against Claude's @\/v1\/messages@ using the
-- subscription-OAuth Bearer flow. Mirrors
-- "LLM.Provider.Anthropic".'askClaude' signature-wise (Either Text
-- Text), so capability-class instances can treat the two
-- interchangeably.
--
-- Flow:
--
-- 1. 'ensureFreshCredentials' — proactively refresh if within 60s of
--    expiry.
-- 2. Build the POST body (reuses 'splitSystem' / 'AnthropicRequest').
-- 3. Send with Bearer header.
-- 4. On 401: refresh ONCE + retry. On persistent 401: return 'Left'
--    with a prompt to re-login.
-- 5. On 2xx: decode 'AnthropicResponse', extract 'firstText'.
askAnthropicOAuth
  :: MonadIO m
  => AnthropicOAuthEnv
  -> [ContentWithRole]
  -> m (Either Text Text)
askAnthropicOAuth env contents = liftIO $ do
  freshness <- ensureFreshCredentials env
  case freshness of
    Left err -> pure (Left err)
    Right () -> do
      currentCreds <- readMVar (envCredentials env)
      firstTry <- postMessages env currentCreds contents
      case firstTry of
        PostOK t             -> pure (Right t)
        PostNoContent        -> pure (Left "Anthropic: response contained no text content block")
        PostDecodeError e    -> pure (Left ("Anthropic OAuth: decode error: " <> e))
        PostHttpException e  -> pure (Left ("Anthropic OAuth: HTTP exception: " <> e))
        PostApiError t       -> pure (Left t)
        PostUnauthorized     -> retryAfterRefresh env contents

-- | Internal: one POST attempt. Doesn't retry; the caller decides.
data PostOutcome
  = PostOK Text                     -- 2xx + first text block
  | PostNoContent                   -- 2xx but response has no text block
  | PostDecodeError Text            -- 2xx but body didn't decode
  | PostApiError Text               -- non-2xx that looked like Anthropic's error body
  | PostUnauthorized                -- 401 — caller may retry after refresh
  | PostHttpException Text          -- network / TLS / timeout

postMessages :: AnthropicOAuthEnv -> Credentials -> [ContentWithRole] -> IO PostOutcome
postMessages env creds contents = do
  reqResult <- try (parseRequest (T.unpack (envAnthropicBaseUrl env <> "/v1/messages")))
    :: IO (Either SomeException Request)
  case reqResult of
    Left e -> pure (PostHttpException (T.pack (show e)))
    Right baseReq -> do
      let (systemText, messages) = splitSystem contents
          body = AnthropicRequest
            { _areq_model      = envModel env
            , _areq_max_tokens = envMaxTokens env
            , _areq_system     = systemText
            , _areq_messages   = messages
            }
          req = baseReq
            { method         = "POST"
            , requestHeaders =
                [ (hAuthorization,     "Bearer " <> TE.encodeUtf8 (credAccessToken creds))
                , ("anthropic-version", TE.encodeUtf8 defaultAnthropicVersion)
                , ("anthropic-beta",    TE.encodeUtf8 defaultAnthropicBeta)
                , (hUserAgent,          TE.encodeUtf8 defaultUserAgent)
                , (hContentType,        "application/json")
                ]
            , requestBody    = RequestBodyLBS (Aeson.encode body)
            }
      httpResult :: Either SomeException (Response BL.ByteString) <- try (httpLbs req (envManager env))
      case httpResult of
        Left e  -> pure (PostHttpException (T.pack (show e)))
        Right r -> pure (classifyPostResponse r)

classifyPostResponse :: Response BL.ByteString -> PostOutcome
classifyPostResponse r
  | code == 401            = PostUnauthorized
  | code >= 200, code < 300 =
      case Aeson.eitherDecode bs :: Either String AnthropicResponse of
        Left e   -> PostDecodeError (T.pack e)
        Right r' -> case firstText r' of
          Just t  -> PostOK t
          Nothing -> PostNoContent
  | otherwise =
      case Aeson.eitherDecode bs :: Either String AnthropicErrorResponse of
        Right errResp -> PostApiError
          ( "Anthropic API error ("
              <> T.pack (show code) <> " "
              <> _aerr_type (_aerrResp_error errResp) <> "): "
              <> _aerr_message (_aerrResp_error errResp)
          )
        Left _ -> PostApiError
          ("Anthropic non-success status " <> T.pack (show code) <> " " <> msg)
  where
    code = statusCode (responseStatus r)
    msg  = TE.decodeUtf8 (statusMessage (responseStatus r))
    bs   = responseBody r

-- | Internal: refresh once + retry the POST. If the retry also 401s
-- OR the refresh grant is rejected, return a user-facing error asking
-- them to re-login.
retryAfterRefresh
  :: AnthropicOAuthEnv
  -> [ContentWithRole]
  -> IO (Either Text Text)
retryAfterRefresh env contents = do
  now <- getCurrentTime
  refreshRes <- modifyMVar (envCredentials env) $ \current -> do
    res <- refreshAccessToken (envManager env) (credRefreshToken current)
    case res of
      Left re -> pure (current, Left re)
      Right tr -> do
        let fresh = applyRefresh now current tr
        atomicWriteCredentials (envCredentialsPath env) fresh
        pure (fresh, Right ())
  case refreshRes of
    Left re -> pure (Left ("Anthropic OAuth: 401 then refresh failed — " <> renderRefreshError re))
    Right () -> do
      freshCreds <- readMVar (envCredentials env)
      retry <- postMessages env freshCreds contents
      case retry of
        PostOK t            -> pure (Right t)
        PostNoContent       -> pure (Left "Anthropic: response contained no text content block")
        PostDecodeError e   -> pure (Left ("Anthropic OAuth: decode error after refresh: " <> e))
        PostHttpException e -> pure (Left ("Anthropic OAuth: HTTP exception after refresh: " <> e))
        PostApiError t      -> pure (Left t)
        PostUnauthorized    -> pure (Left
          "Anthropic OAuth: 401 Unauthorized after token refresh — refresh_token likely revoked. Run `claude login` and retry.")

-- -------------------------------------------------------------------
-- Streaming
-- -------------------------------------------------------------------

-- | Streaming variant of 'askAnthropicOAuth'. Phase B.3. Sets
-- @stream: true@ in the request body and consumes the Server-Sent-
-- Events response incrementally.
--
-- @onChunk@ fires once per @content_block_delta@ 'text_delta' with
-- the delta text — the harness wires it to a @performEventAsync@
-- callback so the TUI re-renders mid-response.
--
-- Returns 'Right' with the full accumulated response text when the
-- stream completes normally ('EventMessageStop'). 'Left' on
-- transport failure, non-2xx status, mid-stream @error@ event, SSE
-- parse failure, or persistent 401 (after one refresh attempt).
--
-- The 401-retry logic mirrors non-streaming 'askAnthropicOAuth': on
-- an initial 401 we refresh once + retry the whole stream request.
-- Partial text accumulated by the first attempt is discarded (we
-- never saw a valid first chunk — the 401 is reported on the
-- Response status before any body is read).
askAnthropicOAuthStream
  :: MonadIO m
  => AnthropicOAuthEnv
  -> [ContentWithRole]
  -> (Text -> IO ())      -- ^ callback per text delta
  -> m (Either Text Text)
askAnthropicOAuthStream env contents onChunk = liftIO $ do
  freshness <- ensureFreshCredentials env
  case freshness of
    Left err -> pure (Left err)
    Right () -> do
      current <- readMVar (envCredentials env)
      firstAttempt <- streamOnce env current contents onChunk
      case firstAttempt of
        StreamResult t      -> pure (Right t)
        StreamUnauthorized  -> retryStreamAfterRefresh env contents onChunk
        StreamErrorMsg err  -> pure (Left err)

-- | Outcome of one streaming attempt. Distinguishes 401 (caller may
-- refresh + retry) from other errors (final).
data StreamOutcome
  = StreamResult Text
  | StreamUnauthorized
  | StreamErrorMsg Text

-- | One streaming request against the configured env + credentials.
-- Builds the body with @stream: true@, opens an @withResponse@, and
-- threads the response body through 'feedBytes' until the stream
-- terminates. Pre-'EventMessageStop' bytes drive 'onChunk'; the
-- accumulated text is the 'StreamResult'.
streamOnce
  :: AnthropicOAuthEnv
  -> Credentials
  -> [ContentWithRole]
  -> (Text -> IO ())
  -> IO StreamOutcome
streamOnce env creds contents onChunk = do
  reqResult <- try (parseRequest (T.unpack (envAnthropicBaseUrl env <> "/v1/messages")))
    :: IO (Either SomeException Request)
  case reqResult of
    Left e -> pure (StreamErrorMsg ("Anthropic OAuth stream: HTTP setup: " <> T.pack (show e)))
    Right baseReq -> do
      let (systemText, messages) = splitSystem contents
          body = Aeson.encode $ Aeson.object
            [ "model"      Aeson..= envModel env
            , "max_tokens" Aeson..= envMaxTokens env
            , "stream"     Aeson..= True
            , "system"     Aeson..= systemText
            , "messages"   Aeson..= map toUserAssistantJSON messages
            ]
          req = baseReq
            { method         = "POST"
            , requestHeaders =
                [ (hAuthorization,     "Bearer " <> TE.encodeUtf8 (credAccessToken creds))
                , ("anthropic-version", TE.encodeUtf8 defaultAnthropicVersion)
                , ("anthropic-beta",    TE.encodeUtf8 defaultAnthropicBeta)
                , (hUserAgent,          TE.encodeUtf8 defaultUserAgent)
                , (hContentType,        "application/json")
                , ("accept",            "text/event-stream")
                ]
            , requestBody    = RequestBodyLBS body
            }
      let runStream = withResponse req (envManager env) $ \resp -> do
            let code = statusCode (responseStatus resp)
                br   = responseBody resp
            if | code == 401 -> pure StreamUnauthorized
               | code >= 200, code < 300 -> consumeStream br onChunk
               | otherwise -> do
                   chunks <- brConsume br
                   let bs = BL.fromStrict (BS.concat chunks)
                   pure $ StreamErrorMsg $ case Aeson.eitherDecode bs :: Either String AnthropicErrorResponse of
                     Right errResp ->
                       "Anthropic API error ("
                         <> T.pack (show code) <> " "
                         <> _aerr_type (_aerrResp_error errResp) <> "): "
                         <> _aerr_message (_aerrResp_error errResp)
                     Left _ ->
                       "Anthropic non-success status "
                         <> T.pack (show code) <> " "
                         <> TE.decodeUtf8 (statusMessage (responseStatus resp))
      try runStream >>= \case
        Left (e :: SomeException) ->
          pure (StreamErrorMsg ("Anthropic OAuth stream: HTTP exception: " <> T.pack (show e)))
        Right outcome -> pure outcome

-- | Render one 'ContentWithRole' as the Anthropic messages-API shape.
-- (Mirrors what 'LLM.Provider.Anthropic' does internally; inlined
-- here to avoid pulling in its servant request type for a single
-- field reshape under streaming.)
toUserAssistantJSON :: ContentWithRole -> Aeson.Value
toUserAssistantJSON cwr =
  let role = case _cwr_role cwr of
        Assistant -> "assistant" :: Text
        _         -> "user"
  in Aeson.object
       [ "role"    Aeson..= role
       , "content" Aeson..= _cwr_content cwr
       ]

-- | Drain a 'BodyReader' through 'feedBytes', dispatching events to
-- the chunk callback and the accumulator. Terminates on
-- 'EventMessageStop' (success), 'StreamParseError' or 'EventError'
-- (failure), or EOF without a stop (treated as success with the
-- bytes accumulated so far — Anthropic may close the connection
-- cleanly without a final 'message_stop' marker, though in practice
-- it does send one).
consumeStream :: BodyReader -> (Text -> IO ()) -> IO StreamOutcome
consumeStream br onChunk = do
  bufRef <- newIORef emptyStreamBuffer
  accRef <- newIORef (mempty :: Text)
  outRef <- newIORef (StreamResult "")   -- overwritten by terminator
  stopRef <- newIORef False
  let processEvents [] = pure ()
      processEvents (e:es) = do
        stop <- readIORef stopRef
        if stop
          then pure ()
          else do
            case e of
              EventContentBlockDelta _ t -> do
                modifyIORef' accRef (<> t)
                onChunk t
              EventMessageStop -> do
                acc <- readIORef accRef
                writeIORef outRef (StreamResult acc)
                writeIORef stopRef True
              StreamParseError why -> do
                writeIORef outRef (StreamErrorMsg
                  ("Anthropic OAuth stream: SSE parse error — " <> why))
                writeIORef stopRef True
              EventError msg -> do
                writeIORef outRef (StreamErrorMsg
                  ("Anthropic OAuth stream: server error event — " <> msg))
                writeIORef stopRef True
              _ -> pure ()
            processEvents es
      drain = do
        stop <- readIORef stopRef
        if stop then pure ()
        else do
          chunk <- brRead br
          if BS.null chunk
            then do
              acc <- readIORef accRef
              writeIORef outRef (StreamResult acc)
              writeIORef stopRef True
            else do
              buf <- readIORef bufRef
              let (buf', events) = feedBytes buf chunk
              writeIORef bufRef buf'
              processEvents events
              drain
  drain
  readIORef outRef

-- | Refresh once + retry a streaming request. Mirrors
-- 'retryAfterRefresh' for the non-streaming path.
retryStreamAfterRefresh
  :: AnthropicOAuthEnv
  -> [ContentWithRole]
  -> (Text -> IO ())
  -> IO (Either Text Text)
retryStreamAfterRefresh env contents onChunk = do
  now <- getCurrentTime
  refreshRes <- modifyMVar (envCredentials env) $ \current -> do
    res <- refreshAccessToken (envManager env) (credRefreshToken current)
    case res of
      Left re  -> pure (current, Left re)
      Right tr -> do
        let fresh = applyRefresh now current tr
        atomicWriteCredentials (envCredentialsPath env) fresh
        pure (fresh, Right ())
  case refreshRes of
    Left re  -> pure (Left ("Anthropic OAuth stream: 401 then refresh failed — " <> renderRefreshError re))
    Right () -> do
      fresh <- readMVar (envCredentials env)
      retry <- streamOnce env fresh contents onChunk
      case retry of
        StreamResult t       -> pure (Right t)
        StreamUnauthorized   -> pure (Left
          "Anthropic OAuth stream: 401 Unauthorized after token refresh — refresh_token likely revoked. Run `claude login` and retry.")
        StreamErrorMsg err   -> pure (Left err)
