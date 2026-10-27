{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | OpenAI interpreter for the @LLM 'OpenAIHttp@ effect. Closes over a 'GPTConfig'
-- (which carries the per-request API key) and dispatches to the servant-client
-- prims in "LLM.Provider.OpenAI". History is injected as an 'Assistant' turn.
module LLM.Effect.OpenAI
  ( runLLMOpenAI
  ) where

import LLM.Effect (LLM (..), runCtx, runCtxTyped)
import LLM.Effect.Memory (Memory)
import LLM.LLM (renderHistory)
import LLM.Provider.OpenAI
  ( GPTConfig
  , askGPTServant
  , askGPTServantMultimodal
  , askGPTServantTools
  , askGPTServantTyped
  )
import LLM.Types (APIProvider (OpenAIHttp), ContentWithRole (..))

import qualified Data.Text as T

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)

runLLMOpenAI
  :: (IOE :> es, Memory :> es)
  => GPTConfig
  -> Eff (LLM 'OpenAIHttp : es) a
  -> Eff es a
runLLMOpenAI cfg = interpret $ \_ -> \case
  Ask contents             -> askGPTServant cfg contents
  AskTyped contents        -> askGPTServantTyped cfg contents
  AskWithContext rc q      -> runCtx injectHistory (askGPTServant cfg) rc q
  AskWithContextTyped rc q -> runCtxTyped injectHistory (askGPTServant cfg) rc q
  AskTools defs msgs       -> askGPTServantTools cfg defs msgs
  -- Multimodal HTTP path: ship image URLs (http(s) or data: URIs) as
  -- 'image_url' content parts alongside a flattened text prompt. The
  -- carrier type for 'OpenAIHttp' is @[Text]@ (URLs); callers with raw
  -- bytes are expected to base64-encode into a data-URI themselves.
  AskMultimodal imgUrls contents ->
    askGPTServantMultimodal cfg imgUrls (flattenContents contents)
  where
    injectHistory histItems = [renderHistory histItems]
    flattenContents = T.intercalate "\n\n" . map _cwr_content
