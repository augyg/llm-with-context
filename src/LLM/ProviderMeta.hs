{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Provider metadata fetched ONCE at setup (the available model list) and held
-- read-only via 'Effectful.Reader.Static.Reader' — the same "decide once, read
-- everywhere" shape as 'LLM.Tooling.ToolSet'.
--
-- The per-provider fetch functions (@fetchClaudeModels@ / @fetchOpenAIModels@)
-- live in the provider modules ("LLM.Provider.Anthropic" / "LLM.Provider.OpenAI")
-- since they need that provider's config + servant client; they return the
-- neutral 'ProviderMeta' defined here. Token counting is deliberately NOT here:
-- it is a live, per-request call, not fetch-once setup data.
module LLM.ProviderMeta
  ( ModelInfo (..)
  , ProviderMeta (..)
  , modelIds
  , hasModel
  , runProviderMeta
  ) where

import qualified Data.Text as T

import Effectful (Eff)
import Effectful.Reader.Static (Reader, runReader)

-- | One advertised model.
data ModelInfo = ModelInfo
  { _modelInfo_id :: T.Text
    -- ^ The model id you pass as @model@ in a request (e.g. @claude-opus-4-8@).
  , _modelInfo_displayName :: Maybe T.Text
    -- ^ Human-facing name when the provider supplies one (Anthropic does;
    -- OpenAI does not).
  } deriving (Show, Eq)

-- | The models a provider reported at setup.
newtype ProviderMeta = ProviderMeta
  { _providerMeta_models :: [ModelInfo]
  } deriving (Show, Eq)

-- | Just the model ids.
modelIds :: ProviderMeta -> [T.Text]
modelIds = map _modelInfo_id . _providerMeta_models

-- | Whether a given model id is in the reported set (e.g. to validate a
-- configured model name at startup).
hasModel :: T.Text -> ProviderMeta -> Bool
hasModel m = elem m . modelIds

-- | Establish the read-only provider metadata for a computation (the
-- "fetch once at setup" result). Just 'runReader' specialised to 'ProviderMeta'.
runProviderMeta :: ProviderMeta -> Eff (Reader ProviderMeta : es) a -> Eff es a
runProviderMeta = runReader
