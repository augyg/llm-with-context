{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Anthropic (Claude) stateless prims over the Messages API
-- (@POST https://api.anthropic.com/v1/messages@) via servant-client.
--
-- Everything configurable (HTTP 'Manager', model, version, endpoint, default
-- token cap) is bundled into 'ClaudeConfig' and passed in — there are no
-- hidden constants in the request path; the defaults live in the explicit,
-- overridable 'defaultClaudeConfig'.
--
-- The context-aware / conversation-history behaviour is NOT here — it lives in
-- the effect layer ("LLM.Effect", "LLM.Effect.Anthropic"), which calls these
-- prims. These are the leaves the interpreters wrap.
--
-- Two Anthropic-specific request-shape differences from OpenAI, handled by
-- 'splitSystem': @system@ is a top-level field (not a message with role
-- @system@), and the first message must be a @user@ turn.
module LLM.Provider.Anthropic
  ( -- * Configuration
    ClaudeConfig (..)
  , defaultClaudeConfig
    -- * Request / response types
  , AnthropicRequest (..)
  , AnthropicResponse (..)
  , AnthropicContentBlock (..)
  , AnthropicErrorResponse (..)
  , AnthropicAPIError (..)
    -- * Prims
  , askClaude
  , askClaudeTyped
  , askClaudeTools
    -- * Provider metadata
  , fetchClaudeModels
    -- * Helpers (exported for reuse / testing)
  , splitSystem
  , firstText
  ) where

import LLM.LLM (askTypedBy, tshow)
import LLM.Provider.Servant (describeClientError, rawBodyText, runJSON)
import LLM.ProviderMeta (ModelInfo (..), ProviderMeta (..))
import LLM.ScrubPrefix (scrubPrefix)
import LLM.Types
  ( APIKey (..)
  , APIProvider (..)
  , Block (..)
  , ContentWithRole (..)
  , GPTAnswer
  , GPTError
  , GPTRole (..)
  , RichMessage (..)
  , ToolDef (..)
  , ToolTurn (..)
  , ToolUse (..)
  )

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Options (..), Value, decode, object, (.=))
import Data.Aeson.TH (deriveJSON)
import Data.List (partition)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Servant.API (Get, Header', JSON, Post, Required, Strict, (:>), ReqBody)
import Servant.Client (BaseUrl (..), ClientError, ClientM, Scheme (Https), client)

-- | Everything the Claude prims need beyond the per-request key and messages.
-- Bundled into one record (threaded by the interpreters in "LLM.Effect")
-- rather than passed as a fistful of arguments most callers don't care about.
data ClaudeConfig = ClaudeConfig
  { _claudeConfig_manager :: Manager
  , _claudeConfig_apiKey :: APIKey 'Anthropic
  , _claudeConfig_model :: T.Text
  , _claudeConfig_version :: T.Text
  , _claudeConfig_baseUrl :: BaseUrl
  , _claudeConfig_maxTokens :: Int
  , _claudeConfig_promptCaching :: Bool
    -- ^ When True, mark the stable prefix of a tool request (the @tools@ array)
    -- with Anthropic's @cache_control: ephemeral@ so it is cached and reused
    -- across calls. Only affects 'askClaudeTools' (OpenAI caches automatically).
  }

-- | Sensible, overridable defaults given the per-request key and a shared
-- 'Manager' (the two values with no universal default). Override any other
-- field with record-update syntax
-- (e.g. @(defaultClaudeConfig key mgr) { _claudeConfig_model = "..." }@).
defaultClaudeConfig :: APIKey 'Anthropic -> Manager -> ClaudeConfig
defaultClaudeConfig key mgr = ClaudeConfig
  { _claudeConfig_manager = mgr
  , _claudeConfig_apiKey = key
  , _claudeConfig_model = "claude-opus-4-8"
  , _claudeConfig_version = "2023-06-01"
  , _claudeConfig_baseUrl = BaseUrl Https "api.anthropic.com" 443 ""
  , _claudeConfig_maxTokens = 4096
  , _claudeConfig_promptCaching = False
  }

-- | The request body for @POST /v1/messages@. @max_tokens@ is mandatory for
-- Anthropic; @system@ is omitted entirely when 'Nothing' (see the derived JSON
-- options below).
data AnthropicRequest = AnthropicRequest
  { _areq_model :: T.Text
  , _areq_max_tokens :: Int
  , _areq_system :: Maybe T.Text
  , _areq_messages :: [ContentWithRole]
  } deriving (Show)

-- | One content block in the response. The answer text lives in blocks whose
-- @type@ is @"text"@; non-text blocks (e.g. @"thinking"@) carry no @text@.
-- | One response content block. @text@ blocks carry @_acb_text@; @tool_use@
-- blocks carry @_acb_id@/@_acb_name@/@_acb_input@. All optional so a single
-- record parses every block shape.
data AnthropicContentBlock = AnthropicContentBlock
  { _acb_type :: T.Text
  , _acb_text :: Maybe T.Text
  , _acb_id :: Maybe T.Text
  , _acb_name :: Maybe T.Text
  , _acb_input :: Maybe Value
  } deriving (Show)

-- | The slice of the Messages API response we consume. Extra JSON fields
-- (@id@, @role@, @usage@, …) are ignored by aeson.
newtype AnthropicResponse = AnthropicResponse
  { _aresp_content :: [AnthropicContentBlock]
  } deriving (Show)

-- | The @error@ object inside an Anthropic error response.
data AnthropicAPIError = AnthropicAPIError
  { _aerr_type :: T.Text
  , _aerr_message :: T.Text
  } deriving (Show)

-- | Top-level Anthropic error envelope (@{ "type": "error", "error": {...} }@).
data AnthropicErrorResponse = AnthropicErrorResponse
  { _aerrResp_type :: T.Text
  , _aerrResp_error :: AnthropicAPIError
  } deriving (Show)

-- | One entry of @GET /v1/models@. Extra fields (@type@, @created_at@) ignored.
data AnthropicModel = AnthropicModel
  { _amodel_id :: T.Text
  , _amodel_display_name :: Maybe T.Text
  } deriving (Show)

-- | The @GET /v1/models@ response envelope (@{ "data": [ ... ] }@).
newtype AnthropicModelsResponse = AnthropicModelsResponse
  { _amodels_data :: [AnthropicModel]
  } deriving (Show)

deriveJSON ((scrubPrefix "_acb_") { omitNothingFields = True }) ''AnthropicContentBlock
deriveJSON (scrubPrefix "_aresp_") ''AnthropicResponse
deriveJSON (scrubPrefix "_aerr_") ''AnthropicAPIError
deriveJSON (scrubPrefix "_aerrResp_") ''AnthropicErrorResponse
deriveJSON ((scrubPrefix "_areq_") { omitNothingFields = True }) ''AnthropicRequest
deriveJSON ((scrubPrefix "_amodel_") { omitNothingFields = True }) ''AnthropicModel
deriveJSON (scrubPrefix "_amodels_") ''AnthropicModelsResponse

-- | The servant description of @POST /v1/messages@.
type MessagesAPI =
  "v1" :> "messages"
    :> Header' '[Required, Strict] "x-api-key" T.Text
    :> Header' '[Required, Strict] "anthropic-version" T.Text
    :> ReqBody '[JSON] AnthropicRequest
    :> Post '[JSON] AnthropicResponse

messagesClient :: T.Text -> T.Text -> AnthropicRequest -> ClientM AnthropicResponse
messagesClient = client (Proxy :: Proxy MessagesAPI)

-- | Pull 'System'-role entries out of a message list and join their contents
-- into a single top-level @system@ string, leaving the 'User'/'Assistant'
-- turns as the @messages@ array.
splitSystem :: [ContentWithRole] -> (Maybe T.Text, [ContentWithRole])
splitSystem cwrs =
  let (systemParts, messages) = partition (isSystem . _cwr_role) cwrs
      systemText = case systemParts of
        [] -> Nothing
        _ -> Just $ T.intercalate "\n\n" (fmap _cwr_content systemParts)
  in (systemText, messages)
  where
    isSystem System = True
    isSystem _ = False

-- | The text of the first @"text"@ content block in a response, if any.
firstText :: AnthropicResponse -> Maybe T.Text
firstText =
  listToMaybe . mapMaybe _acb_text . filter ((== "text") . _acb_type) . _aresp_content

-- | Render a 'ClientError' from the Messages API into a user-facing message,
-- decoding Anthropic's structured error body when present (so an invalid key
-- surfaces as Anthropic's own 401 explanation).
renderAnthropicError :: ClientError -> T.Text
renderAnthropicError e = case describeClientError e of
  Right msg -> "Anthropic: " <> msg
  Left (code, body) ->
    let niceMessage = (_aerr_message . _aerrResp_error) <$> decode body
    in "Anthropic API error (" <> tshow code <> "): "
         <> fromMaybe (rawBodyText body) niceMessage

-- | Stateless Claude request: send @contents@ (with any 'System' turns hoisted
-- to the top-level @system@ field) and return the first text block, or a
-- rendered error. The token cap is the globally-decided
-- '_claudeConfig_maxTokens' — not a per-call parameter.
askClaude
  :: MonadIO m
  => ClaudeConfig
  -> [ContentWithRole]
  -> m (Either T.Text T.Text)
askClaude cfg contents = do
  let (systemText, messages) = splitSystem contents
      body = AnthropicRequest
        { _areq_model = _claudeConfig_model cfg
        , _areq_max_tokens = _claudeConfig_maxTokens cfg
        , _areq_system = systemText
        , _areq_messages = messages
        }
  result <- runJSON (_claudeConfig_manager cfg) (_claudeConfig_baseUrl cfg)
    (messagesClient (T.strip (unAPIKey (_claudeConfig_apiKey cfg))) (_claudeConfig_version cfg) body)
  pure $ case result of
    Left err -> Left (renderAnthropicError err)
    Right resp ->
      maybe (Left "Anthropic: response contained no text content block") Right (firstText resp)

-- | Typed Claude request: ask for the answer as the Haskell type @a@ and decode
-- it. Reuses the transport-agnostic 'askTypedBy' over 'askClaude'.
askClaudeTyped
  :: (MonadIO m, Typeable a, Read a)
  => ClaudeConfig
  -> [ContentWithRole]
  -> m (Either GPTError (GPTAnswer a))
askClaudeTyped cfg = askTypedBy (askClaude cfg)

-- Tool-use translation layer ------------------------------------------------

-- | Same endpoint as 'MessagesAPI', but the body is a hand-built 'Value' so we
-- can include @tools@ and rich @tool_use@/@tool_result@ content blocks without a
-- bespoke request record.
type MessagesToolsAPI =
  "v1" :> "messages"
    :> Header' '[Required, Strict] "x-api-key" T.Text
    :> Header' '[Required, Strict] "anthropic-version" T.Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] AnthropicResponse

messagesToolsClient :: T.Text -> T.Text -> Value -> ClientM AnthropicResponse
messagesToolsClient = client (Proxy :: Proxy MessagesToolsAPI)

-- | Render a tool definition. When @cached@ is set, attach
-- @cache_control: ephemeral@ so Anthropic caches the prompt prefix up to and
-- including this tool (we only set it on the LAST tool, caching the whole
-- @tools@ array).
toolDefJSON :: Bool -> ToolDef -> Value
toolDefJSON cached (ToolDef name desc schema) =
  object $
    ["name" .= name, "description" .= desc, "input_schema" .= schema]
      ++ ["cache_control" .= object ["type" .= ("ephemeral" :: T.Text)] | cached]

-- Anthropic tool conversations use only user/assistant turns.
roleTextAnthropic :: GPTRole -> T.Text
roleTextAnthropic = \case
  Assistant -> "assistant"
  _ -> "user"

blockJSON :: Block -> Value
blockJSON = \case
  BlockText t -> object ["type" .= ("text" :: T.Text), "text" .= t]
  BlockToolUse (ToolUse i n inp) ->
    object ["type" .= ("tool_use" :: T.Text), "id" .= i, "name" .= n, "input" .= inp]
  BlockToolResult tuid content isErr ->
    object
      [ "type" .= ("tool_result" :: T.Text)
      , "tool_use_id" .= tuid
      , "content" .= content
      , "is_error" .= isErr
      ]

richMessageJSON :: RichMessage -> Value
richMessageJSON (RichMessage role blocks) =
  object ["role" .= roleTextAnthropic role, "content" .= map blockJSON blocks]

-- | Reconstruct the assistant turn (text + tool calls) from a response.
responseToToolTurn :: AnthropicResponse -> ToolTurn
responseToToolTurn resp =
  ToolTurn
    { _toolTurn_text = if null texts then Nothing else Just (T.intercalate "\n" texts)
    , _toolTurn_toolUses = uses
    , _toolTurn_assistantBlocks = mapMaybe blockOf blocks
    }
  where
    blocks = _aresp_content resp
    texts = [t | b <- blocks, _acb_type b == "text", Just t <- [_acb_text b]]
    uses =
      [ ToolUse i n inp
      | b <- blocks
      , _acb_type b == "tool_use"
      , Just i <- [_acb_id b]
      , Just n <- [_acb_name b]
      , Just inp <- [_acb_input b]
      ]
    blockOf b = case _acb_type b of
      "text" -> BlockText <$> _acb_text b
      "tool_use" -> BlockToolUse <$> (ToolUse <$> _acb_id b <*> _acb_name b <*> _acb_input b)
      _ -> Nothing

-- | Tool-enabled Claude request: the translation between the Messages API wire
-- shape and our neutral 'ToolDef' / 'RichMessage' / 'ToolTurn' types.
askClaudeTools
  :: MonadIO m
  => ClaudeConfig
  -> [ToolDef]
  -> [RichMessage]
  -> m (Either T.Text ToolTurn)
askClaudeTools cfg defs msgs = do
  let lastIx = length defs - 1
      -- Only the last tool carries the cache_control marker (it caches the
      -- entire tools array, the stable prefix), and only when caching is on.
      toolsJSON =
        [ toolDefJSON (_claudeConfig_promptCaching cfg && i == lastIx) d
        | (i, d) <- zip [0 ..] defs
        ]
      body =
        object
          [ "model" .= _claudeConfig_model cfg
          , "max_tokens" .= _claudeConfig_maxTokens cfg
          , "tools" .= toolsJSON
          , "messages" .= map richMessageJSON msgs
          ]
  result <-
    runJSON (_claudeConfig_manager cfg) (_claudeConfig_baseUrl cfg)
      (messagesToolsClient (T.strip (unAPIKey (_claudeConfig_apiKey cfg))) (_claudeConfig_version cfg) body)
  pure $ case result of
    Left err -> Left (renderAnthropicError err)
    Right resp -> Right (responseToToolTurn resp)

-- Provider metadata ----------------------------------------------------------

-- | @GET /v1/models@.
type ModelsAPI =
  "v1" :> "models"
    :> Header' '[Required, Strict] "x-api-key" T.Text
    :> Header' '[Required, Strict] "anthropic-version" T.Text
    :> Get '[JSON] AnthropicModelsResponse

modelsClient :: T.Text -> T.Text -> ClientM AnthropicModelsResponse
modelsClient = client (Proxy :: Proxy ModelsAPI)

-- | Fetch the available Claude models once (for the read-only
-- 'LLM.ProviderMeta.ProviderMeta' env). A single network call at setup.
fetchClaudeModels :: MonadIO m => ClaudeConfig -> m (Either T.Text ProviderMeta)
fetchClaudeModels cfg = do
  result <- runJSON (_claudeConfig_manager cfg) (_claudeConfig_baseUrl cfg)
    (modelsClient (T.strip (unAPIKey (_claudeConfig_apiKey cfg))) (_claudeConfig_version cfg))
  pure $ case result of
    Left err -> Left (renderAnthropicError err)
    Right resp -> Right $ ProviderMeta
      [ ModelInfo (_amodel_id m) (_amodel_display_name m) | m <- _amodels_data resp ]
