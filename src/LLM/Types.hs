{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Core types for the llm-with-context library.
--
-- Provides request\/response types for both the OpenAI (GPT) and DeepSeek\/Ollama
-- backends, a type-safe phantom-typed 'APIKey', conversation history management
-- via 'MonadGPT' \/ 'MonadDeepSeek', and context-selection strategies
-- ('RelevantContext', 'RelevantContextDS') for stateful multi-turn conversations.
module LLM.Types where

import LLM.ScrubPrefix

import Control.Monad.Trans.State
import Data.Default
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Data.ByteString.Lazy as LBS
import GHC.Generics


data APIProvider = OpenAI | Google | AWS | Anthropic
-- Define the APIKey type with a phantom type parameter
newtype APIKey (api :: APIProvider) = APIKey { unAPIKey :: T.Text }

-- | OpenAI chat-completion request body. Serializes directly to the JSON
-- the @\/v1\/chat\/completions@ endpoint expects.
data GPTRequestBody = GPTRequestBody
  { _gptRequest_model :: T.Text
  -- , _gptRequest_response_format :: GPTResponseFormat
  , _gptRequest_max_tokens :: Maybe Int
  , _gptRequest_messages :: [ContentWithRole]
  } deriving (Show,Generic)

-- | Response format hint: plain text or JSON object.
data GPTType = GPT_Text | GPT_JSON deriving Show

    
-- | Wrapper for the @response_format@ field in the OpenAI API.
data GPTResponseFormat = GPTResponseFormat
  { _gptResponseFormat_type :: GPTType
  } deriving (Generic, Show)


-- | Shorthand for ContentWithRole 
cwr :: GPTRole -> T.Text -> ContentWithRole
cwr = ContentWithRole 

-- | A single message in a conversation, pairing a role with text content.
-- Serializes to @{\"role\": ..., \"content\": ...}@ for both OpenAI and Ollama.
data ContentWithRole = ContentWithRole
  { _cwr_role :: GPTRole
  , _cwr_content :: T.Text
  } deriving (Show, Generic)

-- | The role of a message in the conversation.
data GPTRole
  = System     -- ^ System prompt — sets behaviour, persona, or constraints
  | User       -- ^ User turn — the question or instruction
  | Assistant  -- ^ Assistant turn — prior model responses or few-shot examples
  deriving (Show, Eq, Generic)

instance ToJSON GPTRole where
  toJSON = \case
    System -> String "system"
    User -> String "user"
    Assistant -> String "assistant"

instance FromJSON GPTRole where
  parseJSON = withText "Role" $ \case
    "system" -> pure System
    "user" -> pure User
    "assistant" -> pure Assistant
    x -> fail $ show x



-- | Top-level response from the OpenAI chat-completion endpoint.
data PromptResponse = PromptResponse { id :: T.Text
                                     , object :: T.Text
                                     , created :: Int
                                     , choices :: [ResMessage]
                                     , usage :: Usage
                                     } deriving (Show, Generic)

-- | A single choice inside a 'PromptResponse'.
data ResMessage = ResMessage { message :: ContentWithRole
                             , finish_reason :: T.Text
                             , index :: Int
                             } deriving (Show,Generic)


-- | Token usage statistics returned by OpenAI.
data Usage = Usage { prompt_tokens :: Int
                   , completion_tokens :: Int
                   , total_tokens :: Int
                   } deriving (Show,Generic)



-- | Wrapper for a raw GPT response body (unparsed JSON bytes).
data Content = Content { unContent :: LBS.ByteString } deriving Show

-- | Request body for the OpenAI text-to-speech endpoint.
data TextToSpeechBody = TextToSpeechBody
  { _textToSpeech_model :: T.Text
  , _textToSpeech_voice :: T.Text
  , _textToSpeech_input :: T.Text
  }

-- | Structured error returned inside an OpenAI error response.
data ErrorOpenAI = ErrorOpenAI {
  _errorOpenAI_message :: T.Text,
  _errorOpenAI_type :: T.Text, -- 'type' is a reserved keyword in Haskell, so we use type' or another name
  _errorOpenAI_param :: Maybe T.Text,
  _errorOpenAI_code :: Maybe T.Text
} deriving (Show, Generic)

-- | Top-level OpenAI error envelope (@{\"error\": ...}@).
data ErrorResponseOpenAI = ErrorResponseOpenAI {
  _errorResponseOpenAI_error :: ErrorOpenAI
} deriving (Show, Generic)

-- instance Show e => Show (RequestError' e) where
--   --show (NoAuth s) = "NoAuth " <> s
--   show (RequestErr e) = "RequestErr " <> show e 

-- data DeepSeekRequestBody = DeepSeekRequestBody
--   { _ds_model :: T.Text
--   , _ds_prompt :: T.Text
--   , _ds_stream :: Bool
--   } deriving Generic

-- | Whether the model should return plain text or a JSON object.
data ResponseFormat = AsText | AsJSON deriving (Eq, Show)
instance ToJSON ResponseFormat where
  toJSON = \case
    AsJSON -> String "json_object"
    AsText -> String "text"

instance FromJSON ResponseFormat where
  parseJSON = withText "ResponseFormat" $ \t ->
    case T.unpack t of
      "json_object" -> pure AsJSON
      "text"        -> pure AsText
      _             -> fail $ "Unknown ResponseFormat: " ++ T.unpack t

-- | Request body for the Ollama-hosted DeepSeek chat endpoint.
-- Has a 'Default' instance so you only need to set model and messages.
data DeepSeekRequestBody = DeepSeekRequestBody
  { _deepSeekRequest_messages           :: [ContentWithRole]
  , _deepSeekRequest_model              :: DeepSeekModel
  , _deepSeekRequest_images             :: Maybe T.Text
  -- , _deepSeekRequest_frequency_penalty  :: Double
  -- , _deepSeekRequest_max_tokens         :: Int
  -- , _deepSeekRequest_presence_penalty   :: Double
  -- , _deepSeekRequest_response_format    :: ResponseFormat
  -- , _deepSeekRequest_stop               :: Maybe String
  , _deepSeekRequest_stream             :: Bool
  -- , _deepSeekRequest_stream_options     :: Maybe String
  -- , _deepSeekRequest_temperature        :: Double
  -- , _deepSeekRequest_top_p              :: Double
  -- , _deepSeekRequest_tools              :: Maybe String
  -- , _deepSeekRequest_tool_choice        :: String
  -- , _deepSeekRequest_logprobs           :: Bool
  -- , _deepSeekRequest_top_logprobs       :: Maybe String
  } deriving (Show, Generic)

--instance ToJSON DeepSeekRequestBody 


instance Default DeepSeekRequestBody where
  def = DeepSeekRequestBody
    { _deepSeekRequest_messages          = []
    , _deepSeekRequest_model             = DS_1_5b
    , _deepSeekRequest_images            = Nothing 
    -- , _deepSeekRequest_frequency_penalty = 0
    -- , _deepSeekRequest_max_tokens        = 4096
    -- , _deepSeekRequest_presence_penalty  = 0
    -- , _deepSeekRequest_response_format   = AsText
    -- , _deepSeekRequest_stop              = Nothing
    , _deepSeekRequest_stream            = False
    -- , _deepSeekRequest_stream_options    = Nothing
    -- , _deepSeekRequest_temperature       = 1
    -- , _deepSeekRequest_top_p             = 1
    -- , _deepSeekRequest_tools             = Nothing
    -- , _deepSeekRequest_tool_choice       = ""
    -- , _deepSeekRequest_logprobs          = False
    -- , _deepSeekRequest_top_logprobs      = Nothing
    }

-- | Available DeepSeek-R1 model sizes for local Ollama inference.
-- Ordered smallest to largest; serializes to e.g. @\"deepseek-r1:7b\"@.
data DeepSeekModel = DS_1_5b | DS_7b | DS_8b | DS_14b | DS_32b | DS_70b | DS_671b deriving (Eq, Ord, Enum, Show, Generic)
instance ToJSON DeepSeekModel where
  toJSON = \case
    DS_1_5b -> "deepseek-r1:1.5b"
    DS_7b -> "deepseek-r1:7b"
    DS_8b ->  "deepseek-r1:8b"
    DS_14b -> "deepseek-r1:14b"
    DS_32b -> "deepseek-r1:32b"
    DS_70b -> "deepseek-r1:70b"
    DS_671b -> "deepseek-r1:671b" -- just in case `\_o_/`

instance FromJSON DeepSeekModel where
  parseJSON = withText "DeepSeekModel" $ \t ->
    case T.unpack t of
      "deepseek-r1:1.5b" -> pure DS_1_5b
      "deepseek-r1:7b"   -> pure DS_7b
      "deepseek-r1:8b"   -> pure DS_8b
      "deepseek-r1:14b"  -> pure DS_14b
      "deepseek-r1:32b"  -> pure DS_32b
      "deepseek-r1:70b"  -> pure DS_70b
      "deepseek-r1:671b" -> pure DS_671b
      _                  -> fail $ "Unknown DeepSeekModel: " ++ T.unpack t
--   1.5b
-- 7b
-- 8b
-- 14b
-- 32b
-- 70b
-- 671b


-- | Response from the Ollama @\/api\/chat@ endpoint (non-streaming).
data DeepSeekResponse = DeepSeekResponse
  { _deepSeekResponse_model              :: String
  , _deepSeekResponse_created_at         :: String
  --, _deepSeekResponse_response           :: String
  , _deepSeekResponse_message           :: ContentWithRole
  , _deepSeekResponse_done               :: Bool
  , _deepSeekResponse_done_reason        :: String
  , _deepSeekResponse_context            :: Maybe [Int]
  } deriving (Show, Generic)



-- | Monad transformer for DeepSeek\/Ollama conversations.
-- Uses 'StateT' with a 'ConversationHistoryDeepSeek' to accumulate tagged Q&A turns.
type MonadDeepSeek m a = StateT ConversationHistoryDeepSeek m a

-- | Accumulated Q&A pairs from a conversation, most-recent first.
type ConversationHistory = [ConvoQuery T.Text]
--type ConversationHistoryCWR = [GPTQuery ContentWithRole]

-- | Accumulated tagged message lists from a DeepSeek conversation.
type ConversationHistoryDeepSeek = [(TagDS, [ContentWithRole])]

-- q == x
-- a == x ++ "-answer"


-- | A single tagged question\/answer pair stored in conversation history.
data ConvoQuery a = ConvoQuery
  { _convoQuery_tag :: Tag
  , _convoQuery_question :: ConvoQuestion
  , _convoQuery_answer ::  ConvoAnswer a
  }

-- | User-defined label for a conversation turn, used by 'RelevantContext'
-- to select which history items to include as context.
newtype Tag = Tag { unTag :: T.Text } deriving (Eq,Show)

-- | Tag for DeepSeek conversation items; 'isAnswerDS' distinguishes
-- question entries from answer entries in the flat history list.
data TagDS = TagDS { unTagDS :: T.Text, isAnswerDS :: Bool } deriving (Eq,Show, Generic)

instance ToJSON TagDS
instance FromJSON TagDS

-- | The prompt messages for a single conversation question.
newtype ConvoQuestion = ConvoQuestion [ContentWithRole]

-- | Wrapper for a parsed answer from a model.
newtype ConvoAnswer a = ConvoAnswer { unConvoAnswer :: a } deriving (Generic, Show)

instance ToJSON a => ToJSON (ConvoAnswer a)
instance FromJSON a => FromJSON (ConvoAnswer a)

-- | Convenience alias — DeepSeek answers carry the full 'ContentWithRole'.
type DeepSeekAnswer = ConvoAnswer ContentWithRole

-- | A tool definition advertised to the model (provider-neutral). The schema is
-- a JSON-Schema 'Value'; each provider serialises it into its own @tools@ shape.
data ToolDef = ToolDef
  { _toolDef_name :: T.Text
  , _toolDef_description :: T.Text
  , _toolDef_inputSchema :: Value
  } deriving (Show, Generic)

-- | A tool call the model asked for.
data ToolUse = ToolUse
  { _toolUse_id :: T.Text
  , _toolUse_name :: T.Text
  , _toolUse_input :: Value
  } deriving (Show, Generic)

-- | A content block in a tool-capable message: assistant text, an assistant
-- tool call, or a user-supplied tool result.
data Block
  = BlockText T.Text
  | BlockToolUse ToolUse
  | BlockToolResult T.Text T.Text Bool -- ^ tool_use id, result content, is-error
  deriving (Show, Generic)

-- | A message in the tool-use conversation (richer than 'ContentWithRole',
-- which is text-only).
data RichMessage = RichMessage
  { _richMessage_role :: GPTRole
  , _richMessage_blocks :: [Block]
  } deriving (Show, Generic)

-- | The assistant's turn from a tool-capable ask: any text it emitted, the tool
-- calls it requested (empty = it's done), and the raw assistant blocks to
-- append to the conversation before sending tool results back.
data ToolTurn = ToolTurn
  { _toolTurn_text :: Maybe T.Text
  , _toolTurn_toolUses :: [ToolUse]
  , _toolTurn_assistantBlocks :: [Block]
  } deriving (Show, Generic)

data RelevantContext
  = LastN Int                       -- ^ Take the @n@ most recent items
  | Relevants [Tag]                 -- ^ Take items matching specific tags
  | LastNRelevant Int (Tag -> Bool) -- ^ Take the @n@ most recent items whose tag matches a predicate

-- | Same as 'RelevantContext' but for the DeepSeek conversation history.
data RelevantContextDS
  = LastN_DS Int
  | Relevants_DS [TagDS]
  | LastNRelevant_DS Int (TagDS -> Bool)

-- 3 tags:
--   html-1
--   html-2
--   xml-2342

-- | The prompt messages for a single question to DeepSeek.
newtype DeepSeekQuestion = DeepSeekQuestion [ContentWithRole]

-- | Parsed DeepSeek-R1 response split into @\<think\>@ reasoning and the
-- final answer. Produced by 'LLM.LLM.toThoughtResponse'.
data ThoughtResponse = ThoughtResponse
  { _thoughtResponse_think :: [T.Text]   -- ^ Lines inside @\<think\>...\<\/think\>@
  , _thoughtResponse_answer :: [T.Text]  -- ^ Lines after the think block
  } deriving Show

instance ToJSON GPTType where
  toJSON = toJSON . T.pack . \case
    GPT_Text -> "text"
    GPT_JSON -> "json_object"

instance FromJSON GPTType where
  parseJSON = withText "GPTType" $ \case
    "text" -> pure GPT_Text
    "json_object" -> pure GPT_JSON
    t -> fail . T.unpack $ "unknown GPT type" <> t

-- | Error response from the Ollama API.
data OllamaError = OllamaError { _ollama_error :: T.Text }

-- | A single content block in an Anthropic response (type + text).
data AnthropicContent = AnthropicContent
  { _anthropicContent_type :: T.Text
  , _anthropicContent_text :: T.Text
  } deriving (Show, Generic)

-- | Top-level response from the Anthropic Messages API.
data AnthropicResponse = AnthropicResponse
  { _anthropicResponse_id      :: T.Text
  , _anthropicResponse_content :: [AnthropicContent]
  , _anthropicResponse_model   :: T.Text
  } deriving (Show, Generic)

deriveJSON (scrubPrefix "_anthropicContent_") ''AnthropicContent
deriveJSON (scrubPrefix "_anthropicResponse_") ''AnthropicResponse
deriveJSON (scrubPrefix "_errorOpenAI_") ''ErrorOpenAI
deriveJSON (scrubPrefix "_errorResponseOpenAI_") ''ErrorResponseOpenAI
deriveJSON (scrubPrefix "_textToSpeech_") ''TextToSpeechBody
deriveJSON (scrubPrefix "_gptResponseFormat_") ''GPTResponseFormat
deriveJSON (scrubPrefix "_cwr_") ''ContentWithRole
deriveJSON (scrubPrefix "_deepSeekRequest_") ''DeepSeekRequestBody
deriveJSON (scrubPrefix "_deepSeekResponse_") ''DeepSeekResponse
deriveJSON (scrubPrefix "_ollama_") ''OllamaError

instance FromJSON Usage
instance ToJSON Usage
instance FromJSON PromptResponse
instance ToJSON PromptResponse
instance FromJSON ResMessage
instance ToJSON ResMessage


deriveJSON (scrubPrefix "_gptRequest_") ''GPTRequestBody
