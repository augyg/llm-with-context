{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | servant-client OpenAI/ChatGPT stateless prims. The original http-client
-- functions in "LLM.LLM" ('LLM.LLM.askGPT' etc.) are left intact; these mirror
-- them over servant-client. Context/history behaviour lives in the effect layer
-- ("LLM.Effect", "LLM.Effect.OpenAI"), which calls these prims.
--
-- The request/response/error JSON shapes are the ones already in "LLM.Types"
-- ('GPTRequestBody', 'PromptResponse', 'ErrorResponseOpenAI'). Everything
-- configurable (manager, model, endpoint, token cap and the @+slack@ nudge) is
-- bundled into 'GPTConfig'; the @+50@ that used to be an unexplained literal is
-- now the named, overridable '_gptConfig_maxTokensSlack' field.
module LLM.Provider.OpenAI
  ( -- * Configuration
    GPTConfig (..)
  , defaultGPTConfig
    -- * Prims
  , askGPTServant
  , askGPTServantTyped
  , askGPTServantTools
    -- * Provider metadata
  , fetchOpenAIModels
  ) where

import LLM.LLM (askTypedBy, gptModel, tshow)
import LLM.Provider.Servant (describeClientError, rawBodyText, runJSON)
import LLM.ProviderMeta (ModelInfo (..), ProviderMeta (..))
import LLM.Types
  ( APIKey (..)
  , APIProvider (..)
  , Block (..)
  , ContentWithRole (..)
  , ErrorOpenAI (..)
  , ErrorResponseOpenAI (..)
  , GPTAnswer
  , GPTError
  , GPTRequestBody (..)
  , GPTRole (..)
  , PromptResponse (choices)
  , ResMessage (message)
  , RichMessage (..)
  , ToolDef (..)
  , ToolTurn (..)
  , ToolUse (..)
  , cwr
  )

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Types (Parser, parseEither, withObject, (.:), (.:?))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Manager)
import Servant.API (Get, Header', JSON, Post, Required, Strict, (:>), ReqBody)
import Servant.Client (BaseUrl (..), ClientError, ClientM, Scheme (Https), client)

-- | Everything the OpenAI prims need beyond the messages: the per-request key
-- lives here too (globally decided for the request scope, not a per-call arg).
data GPTConfig = GPTConfig
  { _gptConfig_manager :: Manager
  , _gptConfig_apiKey :: APIKey 'OpenAI
  , _gptConfig_model :: T.Text
  , _gptConfig_baseUrl :: BaseUrl
  , _gptConfig_maxTokens :: Int
    -- | OpenAI tends to overshoot a hard cap as the prompt grows, so the
    -- request asks for @maxTokens + slack@. Was an unexplained @50@ inline in
    -- the original 'LLM.LLM.askGPT'; now a named, overridable knob.
  , _gptConfig_maxTokensSlack :: Int
  }

-- | Sensible, overridable defaults given the per-request key and a shared
-- 'Manager' (the two values with no universal default).
defaultGPTConfig :: APIKey 'OpenAI -> Manager -> GPTConfig
defaultGPTConfig key mgr = GPTConfig
  { _gptConfig_manager = mgr
  , _gptConfig_apiKey = key
  , _gptConfig_model = gptModel
  , _gptConfig_baseUrl = BaseUrl Https "api.openai.com" 443 ""
  , _gptConfig_maxTokens = 4096
  , _gptConfig_maxTokensSlack = 50
  }

-- | @POST /v1/chat/completions@ with a Bearer token.
type ChatCompletionsAPI =
  "v1" :> "chat" :> "completions"
    :> Header' '[Required, Strict] "Authorization" T.Text
    :> ReqBody '[JSON] GPTRequestBody
    :> Post '[JSON] PromptResponse

chatCompletionsClient :: T.Text -> GPTRequestBody -> ClientM PromptResponse
chatCompletionsClient = client (Proxy :: Proxy ChatCompletionsAPI)

renderOpenAIError :: ClientError -> T.Text
renderOpenAIError e = case describeClientError e of
  Right msg -> "OpenAI: " <> msg
  Left (code, body) ->
    let niceMessage = (_errorOpenAI_message . _errorResponseOpenAI_error) <$> decode body
    in "OpenAI API error (" <> tshow code <> "): "
         <> fromMaybe (rawBodyText body) niceMessage

-- | servant-client equivalent of 'LLM.LLM.askGPT'. Same prompt-shaping logic
-- (the limit-reminder system message and the @+slack@ nudge), with the token
-- cap and slack taken from 'GPTConfig' rather than baked in.
askGPTServant
  :: MonadIO m
  => GPTConfig
  -> [ContentWithRole]
  -> m (Either T.Text T.Text)
askGPTServant cfg contents = do
  let maxT = _gptConfig_maxTokens cfg
      slack = _gptConfig_maxTokensSlack cfg
      promptLimitReminder =
        [cwr System $ "Please limit your response to about " <> tshow maxT <> " tokens"]
      body = GPTRequestBody (_gptConfig_model cfg) (Just (maxT + slack)) (promptLimitReminder <> contents)
      authHeader = "Bearer " <> (T.strip . unAPIKey $ _gptConfig_apiKey cfg)
  result <- runJSON (_gptConfig_manager cfg) (_gptConfig_baseUrl cfg) (chatCompletionsClient authHeader body)
  pure $ case result of
    Left err -> Left (renderOpenAIError err)
    Right resp -> case choices resp of
      [] -> Left "No choices"
      (firstChoice : _) -> Right (_cwr_content (message firstChoice))

-- | Typed OpenAI request via servant-client. Reuses 'askTypedBy' over
-- 'askGPTServant'.
askGPTServantTyped
  :: (MonadIO m, Typeable a, Read a)
  => GPTConfig
  -> [ContentWithRole]
  -> m (Either GPTError (GPTAnswer a))
askGPTServantTyped cfg = askTypedBy (askGPTServant cfg)

-- Tool-use translation layer ------------------------------------------------

type ChatToolsAPI =
  "v1" :> "chat" :> "completions"
    :> Header' '[Required, Strict] "Authorization" T.Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] Value

chatToolsClient :: T.Text -> Value -> ClientM Value
chatToolsClient = client (Proxy :: Proxy ChatToolsAPI)

toolDefJSONOpenAI :: ToolDef -> Value
toolDefJSONOpenAI (ToolDef name desc schema) =
  object
    [ "type" .= ("function" :: T.Text)
    , "function" .= object ["name" .= name, "description" .= desc, "parameters" .= schema]
    ]

-- OpenAI wants tool-call arguments as a JSON *string*, not an object.
toolCallJSON :: ToolUse -> Value
toolCallJSON (ToolUse i n inp) =
  object
    [ "id" .= i
    , "type" .= ("function" :: T.Text)
    , "function" .= object ["name" .= n, "arguments" .= encodeText inp]
    ]
  where
    encodeText = TE.decodeUtf8 . LBS.toStrict . encode

-- A rich message can expand to several OpenAI messages: a user turn carrying
-- tool results becomes one @role:"tool"@ message per result.
openAIMessages :: RichMessage -> [Value]
openAIMessages (RichMessage role blocks) = case role of
  Assistant ->
    let texts = [t | BlockText t <- blocks]
        toolCalls = [toolCallJSON tu | BlockToolUse tu <- blocks]
        contentVal = if null texts then Null else String (T.intercalate "\n" texts)
        base = ["role" .= ("assistant" :: T.Text), "content" .= contentVal]
    in [object (if null toolCalls then base else base ++ ["tool_calls" .= toolCalls])]
  _ ->
    let texts = [t | BlockText t <- blocks]
        roleStr = case role of System -> "system" :: T.Text; _ -> "user"
        textMsg = [object ["role" .= roleStr, "content" .= T.intercalate "\n" texts] | not (null texts)]
        toolMsgs =
          [ object ["role" .= ("tool" :: T.Text), "tool_call_id" .= tuid, "content" .= content]
          | BlockToolResult tuid content _ <- blocks
          ]
    in textMsg ++ toolMsgs

parseToolCall :: Value -> Parser ToolUse
parseToolCall = withObject "tool_call" $ \o -> do
  i <- o .: "id"
  o .: "function"
    >>= withObject "function"
      ( \fo -> do
          n <- fo .: "name"
          argStr <- fo .: "arguments"
          let inp = fromMaybe Null (decode (LBS.fromStrict (TE.encodeUtf8 argStr)))
          pure (ToolUse i n inp)
      )

parseOpenAIToolTurn :: Value -> Either String ToolTurn
parseOpenAIToolTurn = parseEither $ withObject "response" $ \o -> do
  cs <- o .: "choices"
  case cs of
    [] -> pure (ToolTurn Nothing [] [])
    (choice : _) -> flip (withObject "choice") choice $ \co ->
      co .: "message"
        >>= withObject "message"
          ( \mo -> do
              mcontent <- mo .:? "content"
              toolCalls <- fromMaybe [] <$> mo .:? "tool_calls"
              uses <- traverse parseToolCall toolCalls
              let assistantBlocks = maybe [] (\t -> [BlockText t]) mcontent ++ map BlockToolUse uses
              pure (ToolTurn mcontent uses assistantBlocks)
          )

-- | Tool-enabled OpenAI request: the translation between the chat-completions
-- wire shape and our neutral 'ToolDef' / 'RichMessage' / 'ToolTurn' types.
askGPTServantTools
  :: MonadIO m
  => GPTConfig
  -> [ToolDef]
  -> [RichMessage]
  -> m (Either T.Text ToolTurn)
askGPTServantTools cfg defs msgs = do
  let body =
        object
          [ "model" .= _gptConfig_model cfg
          , "max_tokens" .= _gptConfig_maxTokens cfg
          , "tools" .= map toolDefJSONOpenAI defs
          , "messages" .= concatMap openAIMessages msgs
          ]
      authHeader = "Bearer " <> (T.strip . unAPIKey $ _gptConfig_apiKey cfg)
  result <- runJSON (_gptConfig_manager cfg) (_gptConfig_baseUrl cfg) (chatToolsClient authHeader body)
  pure $ case result of
    Left err -> Left (renderOpenAIError err)
    Right v -> case parseOpenAIToolTurn v of
      Left e -> Left ("OpenAI: failed to parse tool response: " <> T.pack e)
      Right turn -> Right turn

-- Provider metadata ----------------------------------------------------------

type ModelsAPI =
  "v1" :> "models"
    :> Header' '[Required, Strict] "Authorization" T.Text
    :> Get '[JSON] Value

modelsClient :: T.Text -> ClientM Value
modelsClient = client (Proxy :: Proxy ModelsAPI)

-- | Fetch the available OpenAI models once (for the read-only
-- 'LLM.ProviderMeta.ProviderMeta' env). OpenAI reports no display name, so
-- 'ModelInfo' carries the id only.
fetchOpenAIModels :: MonadIO m => GPTConfig -> m (Either T.Text ProviderMeta)
fetchOpenAIModels cfg = do
  let authHeader = "Bearer " <> (T.strip . unAPIKey $ _gptConfig_apiKey cfg)
  result <- runJSON (_gptConfig_manager cfg) (_gptConfig_baseUrl cfg) (modelsClient authHeader)
  pure $ case result of
    Left err -> Left (renderOpenAIError err)
    Right v -> case parseEither parseModels v of
      Left e -> Left ("OpenAI: failed to parse models response: " <> T.pack e)
      Right ms -> Right (ProviderMeta ms)
  where
    parseModels = withObject "models" $ \o ->
      o .: "data" >>= traverse (withObject "model" (\m -> ModelInfo <$> m .: "id" <*> pure Nothing))
