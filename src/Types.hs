{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import ScrubPrefix

import Control.Monad.Trans.State
import Data.Default
import Data.Aeson
import Data.Aeson.TH
import Data.Text as T
import Data.ByteString.Lazy as LBS
import GHC.Generics


data APIProvider = OpenAI | Google | AWS
-- Define the APIKey type with a phantom type parameter
newtype APIKey (api :: APIProvider) = APIKey { unAPIKey :: T.Text }

data GPTRequestBody = GPTRequestBody
  { model :: T.Text
  -- , response_format :: GPTResponseFormat
  , max_tokens :: Maybe Int 
  , messages :: [ContentWithRole]
  -- more exist but we dont need them or use them
  } deriving (Show,Generic)

data GPTType = GPT_Text | GPT_JSON deriving Show

    
data GPTResponseFormat = GPTResponseFormat
  { _gptResponseFormat_type :: GPTType
  } deriving (Generic, Show) 


-- | Shorthand for ContentWithRole 
cwr :: GPTRole -> T.Text -> ContentWithRole
cwr = ContentWithRole 

data ContentWithRole = ContentWithRole
  { _cwr_role :: GPTRole 
  , _cwr_content :: T.Text
  } deriving (Show, Generic)

data GPTRole = System -- we provide some context: "Pretend you are an Interviewer"
             | User -- we ask some question
             | Assistant deriving (Show, Generic)
             -- We tell GPT something : "The assistant messages help store prior responses.
             -- They can also be written by a developer to help give examples of desired behavior."

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



data PromptResponse = PromptResponse { id :: T.Text
                                     , object :: T.Text
                                     , created :: Int
                                     , choices :: [ResMessage]
                                     , usage :: Usage
                                     } deriving (Show, Generic)

data ResMessage = ResMessage { message :: ContentWithRole
                             , finish_reason :: T.Text
                             , index :: Int 
                             } deriving (Show,Generic)


data Usage = Usage { prompt_tokens :: Int
                   , completion_tokens :: Int
                   , total_tokens :: Int
                   } deriving (Show,Generic)



-- Wrapper for Raw GPT Response         
data Content = Content { unContent :: LBS.ByteString } deriving Show

-- default 
data TextToSpeechBody = TextToSpeechBody
  { _textToSpeech_model :: T.Text
  , _textToSpeech_voice :: T.Text
  , _textToSpeech_input :: T.Text
  }

data ErrorOpenAI = ErrorOpenAI {
  _errorOpenAI_message :: T.Text,
  _errorOpenAI_type :: T.Text, -- 'type' is a reserved keyword in Haskell, so we use type' or another name
  _errorOpenAI_param :: Maybe T.Text,
  _errorOpenAI_code :: Maybe T.Text
} deriving (Show, Generic)

-- Define the data structure for the top-level object
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

data DeepSeekModel = DS_1_5b | DS_7b | DS_8b | DS_14b | DS_32b | DS_70b | DS_671b deriving (Eq, Show, Generic)
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


data DeepSeekResponse = DeepSeekResponse
  { _deepSeekResponse_model              :: String
  , _deepSeekResponse_created_at         :: String
  --, _deepSeekResponse_response           :: String
  , _deepSeekResponse_message           :: ContentWithRole
  , _deepSeekResponse_done               :: Bool
  , _deepSeekResponse_done_reason        :: String
  , _deepSeekResponse_context            :: Maybe [Int]
  } deriving (Show, Generic)



-- todo: use readerT for Manager and ApiKey
-- then create this monad as a newtype with getter funcs
-- and a put for history
type MonadGPT m a = StateT ConversationHistory m a
type MonadDeepSeek m a = StateT ConversationHistoryDeepSeek m a 
type ConversationHistory = [GPTQuery T.Text]
--type ConversationHistoryCWR = [GPTQuery ContentWithRole]

type ConversationHistoryDeepSeek = [(TagDS, [ContentWithRole])]

-- q == x
-- a == x ++ "-answer"


data GPTQuery a = GPTQuery
  { _gptQuery_tag :: Tag
  , _gptQuery_question :: GPTQuestion
  , _gptQuery_answer ::  GPTAnswer a
  }
newtype Tag = Tag { unTag :: T.Text } deriving (Eq,Show)
data TagDS = TagDS { unTagDS :: T.Text, isAnswerDS :: Bool } deriving (Eq,Show, Generic)

instance ToJSON TagDS
instance FromJSON TagDS

newtype GPTQuestion = GPTQuestion [ContentWithRole]
newtype GPTAnswer a = GPTAnswer { unGPTAnswer :: a } deriving (Generic, Show)

instance ToJSON a => ToJSON (GPTAnswer a)
instance FromJSON a => FromJSON (GPTAnswer a)

type DeepSeekAnswer = GPTAnswer ContentWithRole

newtype GPTError = GPTError T.Text deriving Show

data RelevantContext
  = LastN Int
  | Relevants [Tag]
  | LastNRelevant Int (Tag -> Bool) -- LastN matching pattern; most general

data RelevantContextDS
  = LastN_DS Int
  | Relevants_DS [TagDS]
  | LastNRelevant_DS Int (TagDS -> Bool) -- LastN matching pattern; most general

-- 3 tags:
--   html-1
--   html-2
--   xml-2342

newtype DeepSeekQuestion = DeepSeekQuestion [ContentWithRole]

                         
data ScriptSectionPlan = ScriptSectionPlan
  { _sectionPlan_title :: T.Text
  , _sectionPlan_wordCount :: Int
  , _sectionPlan_numberOfSeconds :: Int
  , _sectionPlan_outline :: T.Text 
  } deriving (Show,Generic)

data ScriptSectionComplete = ScriptSectionComplete
  { _section_title :: T.Text
  , _section_wordCount :: Int
  , _section_numberOfSeconds :: Int
  , _section_text :: T.Text 
  } deriving (Show,Generic) 

instance Default ScriptSectionPlan where
  def = ScriptSectionPlan
    { _sectionPlan_title = ""
    , _sectionPlan_wordCount = 0
    , _sectionPlan_numberOfSeconds = 0
    , _sectionPlan_outline = ""
    }

instance ToJSON ScriptSectionPlan
instance FromJSON ScriptSectionPlan
instance ToJSON ScriptSectionComplete 
instance FromJSON ScriptSectionComplete

data ThoughtResponse = ThoughtResponse
  { _thoughtResponse_think :: [T.Text]
  , _thoughtResponse_answer :: [T.Text]
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


deriveJSON (scrubPrefix "_errorOpenAI_") ''ErrorOpenAI
deriveJSON (scrubPrefix "_errorResponseOpenAI_") ''ErrorResponseOpenAI
deriveJSON (scrubPrefix "_textToSpeech_") ''TextToSpeechBody
deriveJSON (scrubPrefix "_gptResponseFormat_") ''GPTResponseFormat
deriveJSON (scrubPrefix "_cwr_") ''ContentWithRole
deriveJSON (scrubPrefix "_deepSeekRequest_") ''DeepSeekRequestBody
deriveJSON (scrubPrefix "_deepSeekResponse_") ''DeepSeekResponse

instance FromJSON Usage
instance ToJSON Usage
instance FromJSON PromptResponse
instance ToJSON PromptResponse
instance FromJSON ResMessage
instance ToJSON ResMessage


instance ToJSON GPTRequestBody 
