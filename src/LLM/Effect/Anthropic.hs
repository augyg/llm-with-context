{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import LLM.Provider.Anthropic (ClaudeConfig, askClaude, askClaudeTools, askClaudeTyped)
import LLM.Types (APIProvider (AnthropicHttp), GPTRole (System))

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
  where
    injectHistory histItems = [renderHistoryWithRole System histItems]
