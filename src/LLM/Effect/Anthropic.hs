{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Anthropic interpreter for the @LLM 'AnthropicHttp@ effect. Closes over a
-- 'ClaudeConfig' (which carries the per-request API key) and dispatches to the
-- prims in "LLM.Provider.Anthropic". History is injected as a 'System' turn
-- (Anthropic requires the first message to be a @user@ turn);
-- 'LLM.Provider.Anthropic.splitSystem' hoists it into the top-level @system@
-- field.
module LLM.Effect.Anthropic
  ( runLLMAnthropic
  ) where

import LLM.Effect (LLM (..), runCtx, runCtxTyped)
import LLM.Effect.Memory (Memory)
import LLM.LLM (renderHistoryWithRole)
import LLM.Provider.Anthropic
  ( ClaudeConfig
  , askClaude
  , askClaudeMultimodal
  , askClaudeTools
  , askClaudeTyped
  )
import LLM.Types (APIProvider (AnthropicHttp), ContentWithRole (..), GPTRole (System))

import qualified Data.Text as T

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)

runLLMAnthropic
  :: (IOE :> es, Memory :> es)
  => ClaudeConfig
  -> Eff (LLM 'AnthropicHttp : es) a
  -> Eff es a
runLLMAnthropic cfg = interpret $ \_ -> \case
  Ask contents             -> askClaude cfg contents
  AskTyped contents        -> askClaudeTyped cfg contents
  AskWithContext rc q      -> runCtx injectHistory (askClaude cfg) rc q
  AskWithContextTyped rc q -> runCtxTyped injectHistory (askClaude cfg) rc q
  AskTools defs msgs       -> askClaudeTools cfg defs msgs
  -- Multimodal HTTP path: base64-encode the image bytes inline as
  -- 'image' content blocks alongside a flattened text prompt. The
  -- carrier type for 'AnthropicHttp' is @[ByteString]@; the request-
  -- body shape is built by 'multimodalRequestBody' (see
  -- 'LLM.Provider.Anthropic').
  AskMultimodal imgBytes contents ->
    askClaudeMultimodal cfg imgBytes (flattenContents contents)
  where
    injectHistory histItems = [renderHistoryWithRole System histItems]
    flattenContents = T.intercalate "\n\n" . map _cwr_content
