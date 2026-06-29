{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Interpret the @LLM p@ effect over an 'LLMEnv' 'LLMBackend' via the existing
-- 'askBackend' dispatcher, so one effect surface drives whatever transport the
-- backend describes: the HTTP APIs ('APIWeb'), the @claude@ CLI ('APICLI'), or a
-- pure\/IO mock ('APIMock', for tests). Provider-polymorphic in @p@ — the phantom
-- is nominal here; the 'LLMBackend' picks the real provider. Mirrors
-- 'LLM.Effect.Anthropic.runLLMAnthropic' (history injected as a 'System' turn),
-- swapping the fixed HTTP prim for the backend dispatcher.
--
-- Tool use is not supported through this interpreter: 'askBackend' returns plain
-- text, not structured tool calls, so 'AskTools' returns a 'Left'.
module LLM.Effect.Backend
  ( runLLMBackend
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import qualified Data.Text as T

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)

import LLM.Effect (LLM (..), runCtx, runCtxTyped)
import LLM.Effect.Memory (Memory)
import LLM.LLM (askTypedBy, renderHistoryWithRole)
import LLM.Provider (LLMEnv (..), LLMError (..), askBackend)
import LLM.Types (ContentWithRole, GPTRole (System))

-- | Interpret @LLM p@ by dispatching every text ask through 'askBackend' on the
-- given 'LLMEnv'. Requires 'IOE' (the dispatcher is in 'IO') and 'Memory' (for
-- the context ops).
runLLMBackend
  :: forall p es a. (IOE :> es, Memory :> es)
  => LLMEnv
  -> Eff (LLM p : es) a
  -> Eff es a
runLLMBackend env = interpret $ \_ -> \case
  Ask contents             -> prim contents
  AskTyped contents        -> askTypedBy prim contents
  AskWithContext rc q      -> runCtx injectHistory prim rc q
  AskWithContextTyped rc q -> runCtxTyped injectHistory prim rc q
  AskTools _ _             ->
    pure (Left "LLM.Effect.Backend: tool use is not supported over this transport")
  -- The generic 'LLMBackend' dispatcher is provider-agnostic and has
  -- no slot for the per-provider 'ImageInput' carrier. Surface a
  -- specific error rather than silently dropping the image carrier
  -- and routing text-only — multimodal callers should pick a
  -- provider-specific interpreter (e.g. 'runLLMAnthropicCli').
  AskMultimodal _img _contents ->
    pure (Left "LLM.Effect.Backend: AskMultimodal is not supported by the generic backend \
                \interpreter — use a provider-specific interpreter (e.g. runLLMAnthropicCli).")
  where
    prim :: [ContentWithRole] -> Eff es (Either T.Text T.Text)
    prim contents = liftIO (first renderLLMError <$> askBackend (_llmEnv_backend env) contents)
    injectHistory histItems = [renderHistoryWithRole System histItems]

renderLLMError :: LLMError -> T.Text
renderLLMError (LLMHttpError t)         = t
renderLLMError (LLMParseError t)        = t
renderLLMError (LLMProcessError code t) =
  "LLM process exited with code " <> T.pack (show code) <> ": " <> t
