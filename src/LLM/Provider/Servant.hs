{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared servant-client plumbing for the HTTP-backed providers
-- ('LLM.Provider.OpenAI', 'LLM.Provider.Anthropic'). Keeps the @runClientM@
-- boilerplate and the ClientError dissection in one place so each provider only
-- has to decode its own error-body shape.
module LLM.Provider.Servant
  ( runJSON
  , describeClientError
  , rawBodyText
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (statusCode)
import Servant.Client (BaseUrl, ClientError (..), ClientM, mkClientEnv, runClientM)
import Servant.Client.Core (responseBody, responseStatusCode)

-- | Run a 'ClientM' action against the given base URL using a shared 'Manager'.
runJSON :: MonadIO m => Manager -> BaseUrl -> ClientM a -> m (Either ClientError a)
runJSON mgr base c = liftIO $ runClientM c (mkClientEnv mgr base)

-- | Split a 'ClientError' into the two cases a provider cares about:
--
--   * @Left (status, body)@ — the server returned a non-2xx response. The body
--     is the provider's structured error JSON, which the caller decodes itself.
--   * @Right msg@ — a transport- or decode-level failure with no provider body
--     to parse; already rendered to a human-readable message.
describeClientError :: ClientError -> Either (Int, LBS.ByteString) T.Text
describeClientError = \case
  FailureResponse _ resp ->
    Left (statusCode (responseStatusCode resp), responseBody resp)
  DecodeFailure msg _ -> Right $ "response decode failure: " <> msg
  UnsupportedContentType mediaType _ ->
    Right $ "unsupported response content type: " <> T.pack (show mediaType)
  InvalidContentTypeHeader _ -> Right "invalid content-type header in response"
  ConnectionError e -> Right $ "connection error: " <> T.pack (show e)

-- | Best-effort decode of a raw response body to text, for when the provider's
-- structured error JSON could not be parsed.
rawBodyText :: LBS.ByteString -> T.Text
rawBodyText = T.decodeUtf8 . LBS.toStrict
