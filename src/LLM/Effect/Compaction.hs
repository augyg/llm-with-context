{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Compaction: cross-effect combinators over @LLM p@ AND 'Memory'. Deliberately
-- NOT a 'Memory' operation — summarisation needs the LLM, so the dependency
-- lives here, visible in the constraints. Pin the provider that does the
-- summarising (@compact \@'AnthropicHttp@, or the 'compactAnthropic' alias).
module LLM.Effect.Compaction
  ( summarizeHistory
  , compact
  , compactAnthropic
  , compactOpenAI
  ) where

import LLM.Effect (LLM, ask)
import LLM.Effect.Memory (Memory, forget, note, recallAll, recallPinned, remember)
import LLM.LLM (renderHistory)
import LLM.Types (APIProvider (..), GPTRole (System), cwr)

import Control.Monad (forM_)
import qualified Data.Text as T
import Effectful (Eff, (:>))

-- | Ask provider @p@ to summarise the whole current history (read-only).
summarizeHistory :: forall p es. (LLM p :> es, Memory :> es) => Eff es (Either T.Text T.Text)
summarizeHistory = do
  hist <- recallAll
  ask @p
    [ cwr System "Summarize the conversation so far, preserving key facts, names, and decisions. Be concise."
    , renderHistory hist
    ]

-- | Replace the bulk history with a summary, keeping pinned turns. On a failed
-- summary the history is left untouched. The summary is placed as the oldest
-- (base) turn, with pinned turns layered above it in their original order.
compact :: forall p es. (LLM p :> es, Memory :> es) => Eff es ()
compact = summarizeHistory @p >>= \case
  Left _err -> pure ()
  Right summary -> do
    pinned <- recallPinned
    forget
    note summary
    forM_ (reverse pinned) remember

-- | 'compact' pinned to Anthropic.
compactAnthropic :: (LLM 'AnthropicHttp :> es, Memory :> es) => Eff es ()
compactAnthropic = compact @'AnthropicHttp

-- | 'compact' pinned to OpenAI.
compactOpenAI :: (LLM 'OpenAIHttp :> es, Memory :> es) => Eff es ()
compactOpenAI = compact @'OpenAIHttp
