{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- Orphan instances by design — see 'LLM.Provider.AnthropicHttpStub'.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Stub capability instances for @'OpenAIHttp@.
--
-- The OpenAI HTTP transport's chat-completions wire shape is not wired
-- through the capability classes yet — the existing
-- 'LLM.Provider.OpenAI' / 'LLM.Effect.OpenAI' code handles it via the
-- legacy @LLM 'OpenAIHttp@ effect. This module pins the capability
-- instance shapes so consumers get a typed error on touch (not silent
-- default-shaped output) until the port lands.
--
-- Per the project rule, every deferred-by-Claude implementation gets
-- ONE 'claudeDeferredLogicImplementation' marker per module.
module LLM.Provider.OpenAIHttpStub
  ( -- Capability instances; no other exports.
  ) where

import qualified Data.Text as T

import LLM.Capability (CanMultimodal (..), CanText (..), ImageInput)
import LLM.Types (APIProvider (..))

-- | Single-marker handle for everything this module owes the HTTP
-- backend.
claudeDeferredLogicImplementation :: a
claudeDeferredLogicImplementation = error
  "[LLM.Provider.OpenAIHttpStub] HTTP backend capability instance \
  \not yet implemented (deferred from gs/claude-cli-cfg refactor)."

instance CanText 'OpenAIHttp where
  askText _ = claudeDeferredLogicImplementation

-- | OpenAI's vision API accepts image URLs (and base64 data URIs); we
-- model the carrier as URL-shaped 'T.Text' for now. Pinned in the
-- type family.
type instance ImageInput 'OpenAIHttp = [T.Text]

instance CanMultimodal 'OpenAIHttp where
  askWithImages _ _ = claudeDeferredLogicImplementation
