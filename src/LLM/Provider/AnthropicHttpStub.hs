{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- These instances are orphans by design: the project rule allows at most one
-- 'claudeDeferredLogicImplementation' marker per module, so each deferred
-- HTTP-backend provider gets its OWN module (its instances therefore can't
-- live in the class's module or the type's module).
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Stub capability instances for @'AnthropicHttp@.
--
-- The HTTP transport is not wired through the new capability system
-- yet — the existing 'LLM.Provider.Anthropic' / 'LLM.Effect.Anthropic'
-- code handles HTTP asks via the legacy @LLM 'AnthropicHttp@ effect.
-- This module defines the 'CanText' / 'CanMultimodal' instance shapes
-- so consumers that name capabilities are forced to a typed error here
-- (not silent default-shaped output) until the HTTP backend is ported.
--
-- Per the project rule, every deferred-by-Claude implementation gets
-- ONE 'claudeDeferredLogicImplementation' marker per module — that's
-- exactly why this module exists separately from
-- 'LLM.Provider.OpenAIHttpStub'.
module LLM.Provider.AnthropicHttpStub
  ( -- Capability instances; no other exports.
  ) where

import qualified Data.ByteString as BS

import LLM.Capability (CanMultimodal (..), CanText (..))
import LLM.Types (APIProvider (..))

-- | Single-marker handle for everything this module owes the HTTP
-- backend. Throws verbosely on touch so the call site is obvious in
-- a stack trace.
claudeDeferredLogicImplementation :: a
claudeDeferredLogicImplementation = error
  "[LLM.Provider.AnthropicHttpStub] HTTP backend capability instance \
  \not yet implemented (deferred from gs/claude-cli-cfg refactor)."

instance CanText 'AnthropicHttp where
  askText _ = claudeDeferredLogicImplementation

-- | HTTP-side multimodal carries images as base64-encoded bytes inside
-- the request body's @content@ blocks. Pinned in the type family so
-- consumers can name the carrier (e.g. via @Encoding@) without forcing
-- a lowest-common-denominator on the CLI's path-based input.
instance CanMultimodal 'AnthropicHttp where
  type ImageInput 'AnthropicHttp = [BS.ByteString]
  askWithImages _ _ = claudeDeferredLogicImplementation
