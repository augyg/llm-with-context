{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
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
import Data.Maybe (catMaybes)

-- | The set of deployment surfaces we can target. Each constructor is ONE
-- concrete transport (binary + wire shape), not a vendor. Same vendor with
-- different transports = different constructors (e.g. @'AnthropicHttp@ for
-- the HTTPS Messages API vs @'AnthropicCli@ for the local @claude@ binary).
--
-- Closed sum kind on purpose — the @LLM (p :: APIProvider)@ effect, the
-- 'APIKey' phantom, and the per-provider 'LLM.Capability' instances all
-- pivot on this. Adding a new deployment surface = adding one constructor
-- + the matching capability instances.
data APIProvider
  = AnthropicCli   -- ^ The local @claude@ binary (no API key; capability via subscription).
  | AnthropicHttp  -- ^ @POST https://api.anthropic.com/v1/messages@.
  | OpenAIHttp     -- ^ OpenAI chat-completions HTTP API.
  | GoogleHttp     -- ^ Google's HTTP API (Gemini etc.).
  | AwsBedrock     -- ^ AWS Bedrock.
  | DeepSeekHttp   -- ^ DeepSeek HTTP API (and Ollama-hosted DeepSeek).
  deriving (Show, Eq, Generic)

-- Define the APIKey type with a phantom type parameter. CLI providers
-- like 'AnthropicCli' do not consume an 'APIKey' (their auth is the
-- subscription baked into the binary) — those provider tags simply
-- aren't used here.
newtype APIKey (api :: APIProvider) = APIKey { unAPIKey :: T.Text }
  deriving (Show, Eq, Generic)

-- | Response format hint: plain text or JSON object.
data GPTType = GPT_Text | GPT_JSON deriving Show

-- | Wrapper for the @response_format@ field in the OpenAI API.
data GPTResponseFormat = GPTResponseFormat
  { _gptResponseFormat_type :: GPTType
  } deriving (Generic, Show)

instance ToJSON GPTResponseFormat
instance FromJSON GPTResponseFormat

-- | Shorthand for ContentWithRole 
cwr :: GPTRole -> T.Text -> ContentWithRole
cwr = ContentWithRole 

-- | A single message in a conversation, pairing a role with text content.
-- Serializes to @{"role": ..., "content": ...}@ for both OpenAI and Ollama.
data ContentWithRole = ContentWithRole
  { _cwr_role :: GPTRole
  , _cwr_content :: T.Text
  } deriving (Show, Generic)

instance FromJSON ContentWithRole where
  parseJSON = withObject "ContentWithRole" $ \v ->
    ContentWithRole <$> v .: "role" <*> v .: "content"

instance ToJSON ContentWithRole where
  toJSON (ContentWithRole r c) = Data.Aeson.object ["role" .= r, "content" .= c]

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
data PromptResponse = PromptResponse
  { id :: T.Text
  , object :: T.Text
  , created :: Int
  , choices :: [ResMessage]
  , usage :: Usage
  } deriving (Show, Generic)

-- | A single choice inside a 'PromptResponse'.
data ResMessage = ResMessage
  { message :: ContentWithRole
  , finish_reason :: T.Text
  , index :: Int
  } deriving (Show, Generic)

instance ToJSON ResMessage
instance FromJSON ResMessage where
  parseJSON = withObject "ResMessage" $ \v ->
    ResMessage <$> v .: "message"
               <*> v .:? "finish_reason" .!= ""
               <*> v .:? "index" .!= 0

-- | Token usage statistics returned by OpenAI.
data Usage = Usage
  { prompt_tokens :: Int
  , completion_tokens :: Int
  , total_tokens :: Int
  } deriving (Show, Generic)

instance ToJSON Usage
instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
    Usage <$> v .:? "prompt_tokens" .!= 0
          <*> v .:? "completion_tokens" .!= 0
          <*> v .:? "total_tokens" .!= 0

instance ToJSON PromptResponse
instance FromJSON PromptResponse where
  parseJSON = withObject "PromptResponse" $ \v ->
    PromptResponse <$> v .:? "id" .!= ""
                   <*> v .:? "object" .!= ""
                   <*> v .:? "created" .!= 0
                   <*> v .:? "choices" .!= []
                   <*> v .:? "usage" .!= Usage 0 0 0


-- | Wrapper for a raw GPT response body (unparsed JSON bytes).
data Content = Content { unContent :: LBS.ByteString } deriving Show

-- | Request body for the OpenAI text-to-speech endpoint.
data TextToSpeechBody = TextToSpeechBody
  { _textToSpeech_model :: T.Text
  , _textToSpeech_voice :: T.Text
  , _textToSpeech_input :: T.Text
  } deriving (Show, Generic)

instance ToJSON TextToSpeechBody
instance FromJSON TextToSpeechBody

-- | Structured error returned inside an OpenAI error response.
data ErrorOpenAI = ErrorOpenAI
  { _errorOpenAI_message :: T.Text
  , _errorOpenAI_type :: T.Text
  , _errorOpenAI_param :: Maybe T.Text
  , _errorOpenAI_code :: Maybe T.Text
  } deriving (Show, Generic)

instance ToJSON ErrorOpenAI
instance FromJSON ErrorOpenAI

-- | Top-level OpenAI error envelope (@{"error": ...}@).
data ErrorResponseOpenAI = ErrorResponseOpenAI
  { _errorResponseOpenAI_error :: ErrorOpenAI
  } deriving (Show, Generic)

instance ToJSON ErrorResponseOpenAI
instance FromJSON ErrorResponseOpenAI


-- | Whether the model should return plain text or a JSON object.
data ResponseFormat = AsText | AsJSON deriving (Eq, Show, Generic)

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
  , _deepSeekRequest_stream             :: Bool
  } deriving (Show, Generic)

instance ToJSON DeepSeekRequestBody where
    toJSON = genericToJSON defaultOptions

instance Default DeepSeekRequestBody where
  def = DeepSeekRequestBody
    { _deepSeekRequest_messages          = []
    , _deepSeekRequest_model             = DS_1_5b
    , _deepSeekRequest_images            = Nothing 
    , _deepSeekRequest_stream            = False
    }

-- | Available DeepSeek-R1 model sizes for local Ollama inference.
-- Ordered smallest to largest; serializes to e.g. @"deepseek-r1:7b"@.
data DeepSeekModel = DS_1_5b | DS_7b | DS_8b | DS_14b | DS_32b | DS_70b | DS_671b
  deriving (Eq, Ord, Enum, Show, Generic)

instance ToJSON DeepSeekModel where
  toJSON = \case
    DS_1_5b -> "deepseek-r1:1.5b"
    DS_7b -> "deepseek-r1:7b"
    DS_8b ->  "deepseek-r1:8b"
    DS_14b -> "deepseek-r1:14b"
    DS_32b -> "deepseek-r1:32b"
    DS_70b -> "deepseek-r1:70b"
    DS_671b -> "deepseek-r1:671b"

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

-- | Response from the Ollama @\/api\/chat@ endpoint (non-streaming).
data DeepSeekResponse = DeepSeekResponse
  { _deepSeekResponse_model              :: String
  , _deepSeekResponse_created_at         :: String
  , _deepSeekResponse_message           :: ContentWithRole
  , _deepSeekResponse_done               :: Bool
  , _deepSeekResponse_done_reason        :: String
  , _deepSeekResponse_context            :: Maybe [Int]
  } deriving (Show, Generic)

instance ToJSON DeepSeekResponse
instance FromJSON DeepSeekResponse


-- | Monad transformer for DeepSeek\/Ollama conversations.
-- Uses 'StateT' with a 'ConversationHistoryDeepSeek' to accumulate tagged Q&A turns.
type MonadDeepSeek m a = StateT ConversationHistoryDeepSeek m a
-- | A single entry in conversation history — either a real Q&A exchange
-- or a summary of prior context produced by context compaction.
data ConvoEntry a
  = ConvoExchange (ConvoQuery a)   -- ^ Normal question → answer exchange
  | ConvoSummary Tag a             -- ^ Summarized context (tag + summary text)

-- | Get the tag from any history entry.
entryTag :: ConvoEntry a -> Tag
entryTag (ConvoExchange q) = _convoQuery_tag q
entryTag (ConvoSummary t _) = t

-- TODO: Evolve ConversationHistory from a flat list into a tree structure
-- with summary branches to circumvent deep traversals:
--
--   data LLMTree a
--     = Leaf (ConvoEntry a)
--     | Branch [LLMTree a]
--     | Summary T.Text (LLMTree a)   -- N-token summary that stands in for
--                                     -- its subtree during context selection,
--                                     -- avoiding full traversal of deep histories.
--
-- Open questions:
--   - How does RelevantContext (Gets, LastN, etc.) interact with Summary nodes?
--     Does matching a Summary's tag expand it, or does the summary text suffice?
--   - Who produces the summary? Automatic compaction when a subtree exceeds
--     a token budget, or explicit user/pipeline control?
--   - Should Summary nodes be transparent (expand on demand) or opaque
--     (always use the summary, never recurse)?
--
-- TODO: External memory — read/write persistent memory that lives outside
-- ConversationHistory (e.g. on-disk key-value store, vector DB, or structured
-- files). The LLM context loop should be able to:
--   1. Write facts/conclusions to external memory during a conversation
--   2. Read them back in future conversations (or later in the same one)
--   3. Use RelevantContext / Gets rules to selectively pull external memory
--      entries into the prompt, just like history entries
-- This decouples "what the LLM has learned" from "what fits in the context
-- window" and enables cross-session knowledge accumulation.
--
-- TODO: Haskell package sitemaps as contextual input — generate structured
-- sitemaps (module hierarchy, exported API surface, key type signatures) for
-- Haskell packages and inject them into ConversationHistory as pre-seeded
-- context entries. This gives the LLM a map of available types/functions
-- without dumping entire source files into the prompt. Possible approach:
--   1. Parse .cabal exposed-modules + haddock/hie output for each dep
--   2. Produce a compact sitemap (module → [exported names + signatures])
--   3. Store as ConvoEntry with a well-known tag (e.g. "Sitemap--<package>")
--   4. CtxRule patterns can pin/budget sitemaps like any other context
type ConversationHistory = [ConvoEntry T.Text]
--type ConversationHistoryCWR = [GPTQuery ContentWithRole]

-- | Accumulated Q&A pairs from a conversation, most-recent first.
type ConversationHistory = [ConvoQuery T.Text]

-- | Accumulated tagged message lists from a DeepSeek conversation.
type ConversationHistoryDeepSeek = [(TagDS, [ContentWithRole])]

-- | A single tagged question\/answer pair stored in conversation history.
data ConvoQuery a = ConvoQuery
  { _convoQuery_tag :: Tag
  , _convoQuery_question :: ConvoQuestion
  , _convoQuery_answer ::  ConvoAnswer a
  }

-- | User-defined label for a conversation turn, used by 'RelevantContext'
-- to select which history items to include as context.
newtype Tag = Tag { unTag :: T.Text } deriving (Eq, Show, Generic)

instance ToJSON Tag
instance FromJSON Tag

-- | Tag for DeepSeek conversation items; 'isAnswerDS' distinguishes
-- question entries from answer entries in the flat history list.
data TagDS = TagDS { unTagDS :: T.Text, isAnswerDS :: Bool } deriving (Eq, Show, Generic)
newtype Tag = Tag { unTag :: T.Text } deriving (Eq,Show,Ord)
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

-- | Error wrapper for a conversational ask (parse/transport failure surfaced to
-- the caller). Re-added in the effectful↔upstream merge: the @-X ours@ merge
-- dropped upstream's definition where our tool types were inserted.
newtype ConvoError = ConvoError T.Text deriving (Show)

-- | A tool definition advertised to the model (provider-neutral). The schema is
-- a JSON-Schema 'Value'; each provider serialises it into its own @tools@ shape.
data ToolDef = ToolDef
  { _toolDef_name :: T.Text
  , _toolDef_description :: T.Text
  , _toolDef_inputSchema :: Value
  } deriving (Show, Generic)

-- | Serialize ToolDef to OpenAI's {"type":"function","function":{...}} shape.
instance ToJSON ToolDef where
  toJSON (ToolDef n d schema) = Data.Aeson.object
    [ "type"     .= ("function" :: T.Text)
    , "function" .= Data.Aeson.object
        [ "name"        .= n
        , "description" .= d
        , "parameters"  .= schema
        ]
    ]

instance FromJSON ToolDef

-- | A tool call the model asked for.
data ToolUse = ToolUse
  { _toolUse_id :: T.Text
  , _toolUse_name :: T.Text
  , _toolUse_input :: Value
  } deriving (Show, Generic)

instance ToJSON ToolUse
instance FromJSON ToolUse

-- | A content block in a tool-capable message: assistant text, an assistant
-- tool call, or a user-supplied tool result.
data Block
  = BlockText T.Text
  | BlockToolUse ToolUse
  | BlockToolResult T.Text T.Text Bool -- ^ tool_use id, result content, is-error
  deriving (Show, Generic)

instance ToJSON Block
instance FromJSON Block

-- | A message in the tool-use conversation (richer than 'ContentWithRole',
-- which is text-only).
data RichMessage = RichMessage
  { _richMessage_role :: GPTRole
  , _richMessage_blocks :: [Block]
  } deriving (Show, Generic)

instance ToJSON RichMessage
instance FromJSON RichMessage

-- | The assistant's turn from a tool-capable ask: any text it emitted, the tool
-- calls it requested (empty = it's done), and the raw assistant blocks to
-- append to the conversation before sending tool results back.
data ToolTurn = ToolTurn
  { _toolTurn_text :: Maybe T.Text
  , _toolTurn_toolUses :: [ToolUse]
  , _toolTurn_assistantBlocks :: [Block]
  } deriving (Show, Generic)

instance ToJSON ToolTurn
instance FromJSON ToolTurn

-- | A glob pattern over tag text: @*@ matches any run of characters, @?@ any
-- single character (e.g. @"Run0--PartDetail--*"@).
newtype TagPattern = TagPattern { unTagPattern :: T.Text }
  deriving (Show, Eq, Generic)

instance ToJSON TagPattern
instance FromJSON TagPattern

-- | One context-selection rule: a tag pattern plus an optional recency budget.
-- @crBudget == Nothing@ pins all entries matching the pattern; @Just n@ keeps
-- only the most recent @n@.
data CtxRule = CtxRule
  { crPattern :: TagPattern
  , crBudget :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON CtxRule
instance FromJSON CtxRule

data RelevantContext
  = LastN Int                       -- ^ Take the @n@ most recent items
  | Relevants [Tag]                 -- ^ Take items matching specific tags
  | LastNRelevant Int (Tag -> Bool) -- ^ Take the @n@ most recent items whose tag matches a predicate
  | Gets [CtxRule]                  -- ^ Compose several tag-pattern rules (pin-all / last-n-matching)
  | NoHistory                       -- ^ Include no prior context
  = LastN Int
  | Relevants [Tag]
  | LastNRelevant Int (Tag -> Bool) -- LastN matching pattern; most general
  | Gets [CtxRule]     -- ^ Pattern-based composite: union of budgeted tag-glob filters
  | NoHistory          -- ^ Skip history entirely; caller provides all context in the prompt

-- | A glob pattern over "--"-delimited tag segments.
-- "*" matches any single segment. Literal text matches case-insensitively.
newtype TagPattern = TagPattern { unTagPattern :: [T.Text] }
  deriving (Eq, Show)

-- | Smart constructor: "Run0--PartDetail--*" → TagPattern ["Run0", "PartDetail", "*"]
mkPattern :: T.Text -> TagPattern
mkPattern = TagPattern . T.splitOn "--"

-- | Does a tag match a pattern? Segment count must be equal,
-- "*" matches any segment, literal matches case-insensitively.
matchTag :: TagPattern -> Tag -> Bool
matchTag (TagPattern pats) (Tag t) =
  let segs = T.splitOn "--" t
  in sameLength pats segs
     && and (Prelude.zipWith matchSeg pats segs)
  where
    matchSeg "*" _ = True
    matchSeg p  s  = T.toCaseFold p == T.toCaseFold s
    sameLength [] []         = True
    sameLength (_:xs) (_:ys) = sameLength xs ys
    sameLength _ _           = False

-- | A context selection rule: budget + pattern.
data CtxRule = CtxRule
  { crBudget  :: Maybe Int      -- ^ Nothing = pinned (include ALL matches)
  , crPattern :: TagPattern     -- ^ Which history entries this rule matches
  }

-- | Pin all matches of a pattern (no budget limit).
pin :: T.Text -> CtxRule
pin pat = CtxRule Nothing (mkPattern pat)

-- | Take at most N most-recent matches of a pattern.
lastNMatching :: Int -> T.Text -> CtxRule
lastNMatching n pat = CtxRule (Just n) (mkPattern pat)

-- | Same as 'RelevantContext' but for the DeepSeek conversation history.
data RelevantContextDS
  = LastN_DS Int
  | Relevants_DS [TagDS]
  | LastNRelevant_DS Int (TagDS -> Bool)

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
  deriving (Show, Generic)

instance ToJSON OllamaError
instance FromJSON OllamaError

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


-- ============================================================================
-- OpenAI tool-aware wire-format types
-- ============================================================================

-- | A tool call as it appears in an OpenAI response.
data ToolCall = ToolCall
  { _toolCall_id       :: T.Text
  , _toolCall_type     :: T.Text
  , _toolCall_function :: FunctionCall
  } deriving (Show, Generic)

data FunctionCall = FunctionCall
  { _functionCall_name      :: T.Text
  , _functionCall_arguments :: T.Text  -- ^ JSON string, e.g. "{\"command\":\"ls\"}"
  } deriving (Show, Generic)

-- | Full OpenAI message format (text, assistant tool_calls, or tool result).
-- We use this internally in the OpenAI provider; 'ContentWithRole' stays
-- the simple public API.
data OpenAIMessage = OpenAIMessage
  { _oam_role         :: T.Text
  , _oam_content      :: Maybe T.Text
  , _oam_tool_calls   :: Maybe [ToolCall]
  , _oam_tool_call_id :: Maybe T.Text   -- ^ present when role == "tool"
  } deriving (Show, Generic)

-- | Result of executing a tool, sent back as a "tool" message.
data ToolResult = ToolResult
  { _toolResult_callId  :: T.Text
  , _toolResult_content :: T.Text
  } deriving (Show)

-- | OpenAI chat completion response (tool-aware). Use this instead of
-- 'PromptResponse' when you expect tool_calls in the response.
data OpenAIChatResponse = OpenAIChatResponse
  { _oacr_id      :: T.Text
  , _oacr_object  :: T.Text
  , _oacr_created :: Int
  , _oacr_choices :: [OpenAIChoice]
  , _oacr_usage   :: Usage
  } deriving (Show, Generic)

data OpenAIChoice = OpenAIChoice
  { _oac_message       :: OpenAIMessage
  , _oac_finish_reason :: T.Text
  , _oac_index         :: Int
  } deriving (Show, Generic)

-- JSON instances (omit Nothing fields)
instance ToJSON OpenAIMessage where
  toJSON o = Data.Aeson.object $ catMaybes
    [ Just ("role" .= _oam_role o)
    , ("content" .=) <$> _oam_content o
    , ("tool_calls" .=) <$> _oam_tool_calls o
    , ("tool_call_id" .=) <$> _oam_tool_call_id o
    ]

instance FromJSON OpenAIMessage where
  parseJSON = withObject "OpenAIMessage" $ \v ->
    OpenAIMessage
      <$> v .:  "role"
      <*> v .:? "content"
      <*> v .:? "tool_calls"
      <*> v .:? "tool_call_id"

instance ToJSON ToolCall where
  toJSON tc = Data.Aeson.object
    [ "id"       .= _toolCall_id tc
    , "type"     .= _toolCall_type tc
    , "function" .= _toolCall_function tc
    ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v ->
    ToolCall
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "function"

-- instance ToJSON FunctionCall
-- instance FromJSON FunctionCall
-- Replace these two lines:
-- instance ToJSON FunctionCall
-- instance FromJSON FunctionCall

-- With:
instance ToJSON FunctionCall where
  toJSON fc = Data.Aeson.object
    [ "name"      .= _functionCall_name fc
    , "arguments" .= _functionCall_arguments fc
    ]

instance FromJSON FunctionCall where
  parseJSON = withObject "FunctionCall" $ \v ->
    FunctionCall
      <$> v .: "name"
      <*> v .: "arguments"
      
instance ToJSON OpenAIChatResponse
instance FromJSON OpenAIChatResponse where
  parseJSON = withObject "OpenAIChatResponse" $ \v ->
    OpenAIChatResponse
      <$> v .:? "id" .!= ""
      <*> v .:? "object" .!= ""
      <*> v .:? "created" .!= 0
      <*> v .:? "choices" .!= []
      <*> v .:? "usage" .!= Usage 0 0 0

instance ToJSON OpenAIChoice
instance FromJSON OpenAIChoice where
  parseJSON = withObject "OpenAIChoice" $ \v ->
    OpenAIChoice
      <$> v .:  "message"
      <*> v .:? "finish_reason" .!= ""
      <*> v .:? "index" .!= 0


-- -- | Extend GPTRequestBody with tools.
-- data GPTRequestBody = GPTRequestBody
--   { _gptRequest_model       :: T.Text
--   , _gptRequest_max_tokens  :: Maybe Int
--   , _gptRequest_messages    :: [OpenAIMessage]
--   , _gptRequest_tools       :: Maybe [ToolDef]
--   } deriving (Show, Generic)

-- instance ToJSON GPTRequestBody where
--   toJSON (GPTRequestBody model maxToks msgs tools) = Data.Aeson.object $ catMaybes
--     [ Just ("model" .= model)
--     , ("max_tokens" .=) <$> maxToks
--     , Just ("messages" .= msgs)
--     , ("tools" .=) <$> tools
--     ]

-- instance FromJSON GPTRequestBody where
--   parseJSON = withObject "GPTRequestBody" $ \v ->
--     GPTRequestBody
--       <$> v .:  "model"
--       <*> v .:? "max_tokens"
--       <*> v .:  "messages"
--       <*> v .:? "tools"
data GPTRequestBody = GPTRequestBody
  { _gptRequest_model       :: T.Text
  , _gptRequest_max_tokens  :: Maybe Int
  , _gptRequest_messages    :: [OpenAIMessage]
  , _gptRequest_tools       :: Maybe [ToolDef]
  , _gptRequest_tool_choice :: Maybe T.Text  -- ^ "auto", "none", or "required"
  } deriving (Show, Generic)

instance ToJSON GPTRequestBody where
  toJSON (GPTRequestBody model maxToks msgs tools toolChoice) = Data.Aeson.object $ catMaybes
    [ Just ("model" .= model)
    , ("max_tokens" .=) <$> maxToks
    , Just ("messages" .= msgs)
    , ("tools" .=) <$> tools
    , ("tool_choice" .=) <$> toolChoice
    ]

instance FromJSON GPTRequestBody where
  parseJSON = withObject "GPTRequestBody" $ \v ->
    GPTRequestBody
      <$> v .:  "model"
      <*> v .:? "max_tokens"
      <*> v .:  "messages"
      <*> v .:? "tools"
      <*> v .:? "tool_choice"
      
-- Convert simple ContentWithRole to OpenAIMessage
toOpenAIMessage :: ContentWithRole -> OpenAIMessage
toOpenAIMessage (ContentWithRole r c) = OpenAIMessage
  { _oam_role         = roleToText r
  , _oam_content      = Just c
  , _oam_tool_calls   = Nothing
  , _oam_tool_call_id = Nothing
  }
  where
    roleToText System    = "system"
    roleToText User      = "user"
    roleToText Assistant = "assistant"

-- Convert a tool result to an OpenAIMessage
toolResultToMsg :: ToolResult -> OpenAIMessage
toolResultToMsg (ToolResult cid content) = OpenAIMessage
  { _oam_role         = "tool"
  , _oam_content      = Just content
  , _oam_tool_calls   = Nothing
  , _oam_tool_call_id = Just cid
  }
