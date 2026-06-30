{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import LLM.Capability (CanMultimodal (..), CanText (..))
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
    case res of
      Left e  -> error ("LLM.Provider.OpenAIHttp.askText: " <> T.unpack e)
      Right t -> pure t

-- | Multimodal ask via the active interpreter (typically 'runLLMOpenAI').
instance CanMultimodal 'OpenAIHttp where
  askWithImages imageUrls prompt = do
    res <- send (AskMultimodal imageUrls [ContentWithRole User prompt]
                  :: LLM 'OpenAIHttp (Eff es) (Either T.Text T.Text))
    case res of
      Left e  -> error ("LLM.Provider.OpenAIHttp.askWithImages: " <> T.unpack e)
      Right t -> pure t
