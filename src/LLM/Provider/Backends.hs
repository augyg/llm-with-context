{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module LLM.Provider.Backends
  ( mkOpenAI
  , mkDeepSeek
  , mkClaudeAPI
  , mkClaudeCLI
  ) where

import LLM.Types (APIKey(..), APIProvider(..), DeepSeekModel, unAPIKey)
import LLM.Provider (LLMAPI(..), LLMBackend(..), WebProvider(..))

import Data.Aeson (toJSON, Value(..))
import Data.Maybe (fromJust)
import Network.HTTP.Client (Manager)
import Network.URI (parseURI)
import qualified Data.Text as T

type TokenLimit = Maybe Int

dsModelToText :: DeepSeekModel -> T.Text
dsModelToText m = case toJSON m of
  String s -> s
  _        -> T.pack (show m)

-- | OpenAI backend.
mkOpenAI :: APIKey 'OpenAI -> Manager -> T.Text -> TokenLimit -> LLMBackend
mkOpenAI apiKey mgr modelName tokenLimit = LLMBackend
  { _llmBackend_name = "openai/" <> modelName
  , _llmBackend_api  = APIWeb mgr
      (fromJust $ parseURI "https://api.openai.com/v1/chat/completions")
      (unAPIKey apiKey) modelName tokenLimit ProviderOpenAI
  }

-- | DeepSeek/Ollama backend.
mkDeepSeek :: Manager -> DeepSeekModel -> LLMBackend
mkDeepSeek mgr modelDS = LLMBackend
  { _llmBackend_name = "deepseek/" <> dsModelToText modelDS
  , _llmBackend_api  = APIWeb mgr
      (fromJust $ parseURI "http://localhost:11434/api/chat")
      "" (dsModelToText modelDS) Nothing ProviderOllama
  }

-- | Anthropic Claude API backend.
mkClaudeAPI :: APIKey 'Anthropic -> Manager -> T.Text -> LLMBackend
mkClaudeAPI apiKey mgr modelName = LLMBackend
  { _llmBackend_name = "claude-api/" <> modelName
  , _llmBackend_api  = APIWeb mgr
      (fromJust $ parseURI "https://api.anthropic.com/v1/messages")
      (unAPIKey apiKey) modelName Nothing ProviderAnthropic
  }

-- | Claude CLI backend.
mkClaudeCLI :: T.Text -> LLMBackend
mkClaudeCLI modelName = LLMBackend
  { _llmBackend_name = "claude-cli/" <> modelName
  , _llmBackend_api  = APICLI "claude" modelName
  }
