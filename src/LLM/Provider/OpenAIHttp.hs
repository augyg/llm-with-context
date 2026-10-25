{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- Orphan instances by design — see 'LLM.Provider.AnthropicHttp'.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @'OpenAIHttp@ provider — OpenAI's @POST /v1/chat/completions@ HTTP API.
--
-- Instances delegate to the generic 'LLM' effect; the actual HTTP
-- request is built by @runLLMOpenAI@ (see "LLM.Effect.OpenAI") via the
-- prims in "LLM.Provider.OpenAI".
--
-- The 'ImageInput' carrier is URL-shaped 'T.Text' — OpenAI's vision
-- endpoint takes @image_url@ parts. URLs may be public http(s):
-- links or @data:image/...;base64,...@ data-URIs (callers with raw
-- bytes are expected to base64-encode into a data-URI themselves).
module LLM.Provider.OpenAIHttp
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
-- Bring the 'ImageInput 'OpenAIHttp = [Text]' instance into scope.
import LLM.Provider.OpenAI ()
import LLM.Types
  ( APIProvider (..)
  , ContentWithRole (..)
  , GPTRole (User)
  )

-- | Text-only ask via the active interpreter (typically 'runLLMOpenAI').
instance CanText 'OpenAIHttp where
  askText prompt = do
    res <- send (Ask [ContentWithRole User prompt]
                  :: LLM 'OpenAIHttp (Eff es) (Either T.Text T.Text))
    pure (first TransportError res)

-- | Multimodal ask via the active interpreter (typically 'runLLMOpenAI').
instance CanMultimodal 'OpenAIHttp where
  askWithImages imageUrls prompt = do
    res <- send (AskMultimodal imageUrls [ContentWithRole User prompt]
                  :: LLM 'OpenAIHttp (Eff es) (Either T.Text T.Text))
    pure (first TransportError res)

-- | Structured-output ask via prompt-side schema delivery. The OpenAI
-- HTTP API does have a native @response_format: { type: "json_object" }@
-- mode, but the interpreter ('runLLMOpenAI') does not yet thread that
-- through the generic 'Ask' GADT constructor. Until the interpreter
-- learns to consult the active capability for per-call request
-- shaping, the schema is delivered prompt-side: the example shape is
-- prepended to the user prompt with a clear header. The result is the
-- raw response text — the consumer parses it (honest-API rule: parse
-- errors bubble up at the call site, never swallowed inside the
-- instance, no in-instance retry).
instance CanJsonOutput 'OpenAIHttp where
  type Schema 'OpenAIHttp = String
  askJson schema userPrompt = do
    let body = "Respond with ONLY a JSON object matching this shape:\n"
             <> T.pack schema
             <> "\n\n"
             <> userPrompt
    res <- send (Ask [ContentWithRole User body]
                  :: LLM 'OpenAIHttp (Eff es) (Either T.Text T.Text))
    pure (first TransportError res)
