{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Description: Pre-built 'LLMBackend' constructors for each supported provider.
-- Copyright: (c) lazyLambda, 2024-2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
--
-- Use these to create a backend and pass it to 'LLM.Provider.runLLM':
--
-- @
-- backend <- mkOpenAI apiKey mgr "gpt-4o" Nothing
-- result  <- runLLM (LLMEnv backend) $ askLLM [cwr User "Hello"]
-- @
module LLM.Provider.Backends
  ( mkOpenAI
  , mkDeepSeek
  , mkClaudeAPI
  , mkClaudeCLI
    -- * Phase B.2 subscription-OAuth
  , mkAnthropicOAuth
  , OAuthLoadError (..)
  , ClaudeCliCfg (..)
  , defaultClaudeCliCfg
  ) where

import LLM.Types (APIKey(..), APIProvider(..), DeepSeekModel, unAPIKey)
import LLM.Provider (LLMAPI(..), LLMBackend(..), LLMError(..), WebProvider(..))
import qualified LLM.Provider.AnthropicOAuth.Ask as OAuth
import qualified LLM.Provider.AnthropicOAuth.Credentials as OAuthC

import LLM.StaticURI (staticURI)

import Data.Aeson (toJSON, Value(..))
import Network.HTTP.Client (Manager)
import qualified Data.Text as T

-- | Optional max-token limit for a request. 'Nothing' = provider default.
type TokenLimit = Maybe Int

-- | Render a 'DeepSeekModel' to the text the Ollama API expects.
dsModelToText :: DeepSeekModel -> T.Text
dsModelToText m = case toJSON m of
  String s -> s
  _        -> T.pack (show m)

-- | OpenAI backend.
mkOpenAI :: APIKey 'OpenAIHttp -> Manager -> T.Text -> TokenLimit -> LLMBackend
mkOpenAI apiKey mgr modelName tokenLimit = LLMBackend
  { _llmBackend_name = "openai/" <> modelName
  , _llmBackend_api  = APIWeb mgr
      $(staticURI "https://api.openai.com/v1/chat/completions")
      (unAPIKey apiKey) modelName tokenLimit ProviderOpenAI
  }

-- | DeepSeek/Ollama backend.
mkDeepSeek :: Manager -> DeepSeekModel -> LLMBackend
mkDeepSeek mgr modelDS = LLMBackend
  { _llmBackend_name = "deepseek/" <> dsModelToText modelDS
  , _llmBackend_api  = APIWeb mgr
      $(staticURI "http://localhost:11434/api/chat")
      "" (dsModelToText modelDS) Nothing ProviderOllama
  }

-- | Anthropic Claude API backend.
mkClaudeAPI :: APIKey 'AnthropicHttp -> Manager -> T.Text -> LLMBackend
mkClaudeAPI apiKey mgr modelName = LLMBackend
  { _llmBackend_name = "claude-api/" <> modelName
  , _llmBackend_api  = APIWeb mgr
      $(staticURI "https://api.anthropic.com/v1/messages")
      (unAPIKey apiKey) modelName Nothing ProviderAnthropic
  }

-- | Invocation knobs for the Claude CLI backend.
--
--   * 'ccBin' — the executable path. Default @\"claude\"@ (resolved
--     via the runtime @PATH@). Consumers who want a Nix-stable
--     absolute path can resolve it with @staticWhich@ on their side
--     and pass it in.
--   * 'ccExtraArgs' — extra arguments inserted between the fixed
--     @-p --model M --dangerously-skip-permissions@ flags and the
--     prompt body. Use to pin an alternate model, add a debug flag,
--     etc., without forking 'mkClaudeCLI' or 'askCLI'.
data ClaudeCliCfg = ClaudeCliCfg
  { ccBin       :: FilePath
  , ccExtraArgs :: [String]
  } deriving (Show, Eq)

-- | Default: @\"claude\"@ on @PATH@, no extra args.
defaultClaudeCliCfg :: ClaudeCliCfg
defaultClaudeCliCfg = ClaudeCliCfg
  { ccBin       = "claude"
  , ccExtraArgs = []
  }

-- | Claude CLI backend. Pass 'defaultClaudeCliCfg' if you don't need
-- to override the binary path or supply extra args.
mkClaudeCLI :: ClaudeCliCfg -> T.Text -> LLMBackend
mkClaudeCLI cfg modelName = LLMBackend
  { _llmBackend_name = "claude-cli/" <> modelName
  , _llmBackend_api  = APICLI (ccBin cfg) modelName (ccExtraArgs cfg)
  }

-- | Re-export of 'OAuthC.LoadError' under a shorter name so callers
-- that don't want the submodule-import noise can pattern-match on
-- it. The constructors remain in "LLM.Provider.AnthropicOAuth.Credentials".
type OAuthLoadError = OAuthC.LoadError

-- | Phase B.2: subscription-OAuth Anthropic backend. Reads
-- @~/.claude/.credentials.json@, authenticates with the user's
-- @claudeAiOauth.accessToken@, refreshes on expiry / 401, and
-- atomically rewrites the file when a refresh succeeds.
--
-- The returned 'LLMBackend' uses the 'APIClosure' constructor so the
-- closure-over-env state (MVar for serialised refresh) stays local
-- to the backend without 'LLM.Provider' needing to import the
-- OAuth-specific modules.
--
-- @
-- mgr   <- newTlsManager
-- cPath <- defaultCredentialsPath
-- eback <- mkAnthropicOAuth mgr cPath "claude-opus-4-7" 4096
--            "https://api.anthropic.com"
-- case eback of
--   Left err  -> error ("Need `claude login` first: " <> show err)
--   Right be  -> runLLM (mkLLMEnv be) $ askLLM [cwr User "Hello"]
-- @
mkAnthropicOAuth
  :: Manager
  -> FilePath   -- ^ credentials path (@defaultCredentialsPath@ for default)
  -> T.Text     -- ^ model name (e.g. @"claude-opus-4-7"@)
  -> Int        -- ^ @max_tokens@ output cap
  -> T.Text     -- ^ Anthropic base URL (typically @"https://api.anthropic.com"@)
  -> IO (Either OAuthLoadError LLMBackend)
mkAnthropicOAuth mgr credPath modelName maxToks baseUrl = do
  envRes <- OAuth.newAnthropicOAuthEnv mgr credPath modelName maxToks baseUrl
  case envRes of
    Left e    -> pure (Left e)
    Right env -> pure $ Right LLMBackend
      { _llmBackend_name = "anthropic-oauth/" <> modelName
      , _llmBackend_api  = APIClosure $ \msgs -> do
          r <- OAuth.askAnthropicOAuth env msgs
          pure $ case r of
            Left err -> Left (LLMHttpError err)
            Right t  -> Right t
      }
