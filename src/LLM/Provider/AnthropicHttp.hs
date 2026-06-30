{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- The CanText / CanMultimodal instances for 'AnthropicHttp are orphans by
-- design: the class lives in 'LLM.Capability' and the provider tag lives in
-- 'LLM.Types', so neither is the right home. Each HTTP provider owns its own
-- module so the instance + ImageInput carrier sit together.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @'AnthropicHttp@ provider — Anthropic's @POST /v1/messages@ HTTP API.
--
-- The instances dispatch every capability through the generic 'LLM'
-- effect's GADT constructors ('Ask' for text-only, 'AskMultimodal' for
-- vision); the actual HTTP request is built by the interpreter
-- @runLLMAnthropic@ (see "LLM.Effect.Anthropic") via the prims in
-- "LLM.Provider.Anthropic" — same shape as the @'AnthropicCli@ pattern
-- in "LLM.Provider.AnthropicCli".
--
-- The 'ImageInput' carrier for the HTTP transport is raw image bytes
-- (@[ByteString]@). The interpreter base64-encodes them into Anthropic's
-- @image@ content blocks. Media type is inferred via 'detectImageMediaType'
-- (PNG/JPEG/GIF/WebP magic-byte sniff, defaults to @image/png@).
module LLM.Provider.AnthropicHttp
  ( -- Capability instances; no other exports.
  ) where

import qualified Data.Text as T

import Effectful (Eff)
import Effectful.Dispatch.Dynamic (send)

import LLM.Capability
  ( CanJsonOutput (..)
  , CanMultimodal (..)
  , CanText (..)
  , TransportError (..)
  )
import Data.Bifunctor (first)
import LLM.Effect (LLM (..))
-- Bring the 'ImageInput 'AnthropicHttp = [ByteString]' instance into scope.
import LLM.Provider.Anthropic ()
import LLM.Types
  ( APIProvider (..)
  , ContentWithRole (..)
  , GPTRole (User)
  )

-- | Text-only ask: wrap the prompt in a single User turn and dispatch
-- through the 'Ask' GADT constructor. The active interpreter
-- ('runLLMAnthropic') routes this to @POST /v1/messages@. Failure
-- surfaces as a runtime error — the capability-class signature returns
-- 'T.Text' (no Either); the honest @Either@ path is 'ask' from
-- "LLM.Effect".
instance CanText 'AnthropicHttp where
  askText prompt = do
    res <- send (Ask [ContentWithRole User prompt]
                  :: LLM 'AnthropicHttp (Eff es) (Either T.Text T.Text))
    pure (first TransportError res)

-- | Multimodal ask: dispatch through 'AskMultimodal', carrying the
-- image-bytes payload. The interpreter ('runLLMAnthropic') builds the
-- multimodal request body via 'multimodalRequestBody' /
-- 'askClaudeMultimodal' in "LLM.Provider.Anthropic". Failure surfaces
-- as a runtime error (same shape as 'askText').
instance CanMultimodal 'AnthropicHttp where
  askWithImages imageBytes prompt = do
    res <- send (AskMultimodal imageBytes [ContentWithRole User prompt]
                  :: LLM 'AnthropicHttp (Eff es) (Either T.Text T.Text))
    pure (first TransportError res)

-- | Structured-output ask via prompt-side schema delivery. The native
-- Anthropic HTTP API does not have a JSON-mode request flag (unlike
-- OpenAI's @response_format@), so the schema text (see
-- "LLM.JsonExample") is prepended to the user prompt with a
-- "Respond with ONLY a JSON object matching this shape:" header. The
-- result is the raw response text — the consumer parses it (honest-API
-- rule: parse errors bubble up at the call site, never swallowed
-- inside the instance, no in-instance retry).
instance CanJsonOutput 'AnthropicHttp where
  type Schema 'AnthropicHttp = String
  askJson schema userPrompt = do
    let body = "Respond with ONLY a JSON object matching this shape:\n"
             <> T.pack schema
             <> "\n\n"
             <> userPrompt
    res <- send (Ask [ContentWithRole User body]
                  :: LLM 'AnthropicHttp (Eff es) (Either T.Text T.Text))
    pure (first TransportError res)
