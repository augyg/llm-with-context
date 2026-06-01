{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Constrained generation via self-repair: ask, validate the parsed result
-- against caller-supplied invariants, and on failure feed the violations back
-- into the prompt and retry. The generic mechanism — the 'SemanticError' class
-- and the 'askValidated' loop — lives here; each consumer supplies its own error
-- ADT + validator (@a -> [e]@). This is the structured-output sibling of a plain
-- retry.
module LLM.Effect.Validate
  ( SemanticError (..)
  , askValidated
  ) where

import qualified Data.Text as T

import Effectful (Eff, (:>))

import LLM.Effect (LLM, ask)
import LLM.Effect.Memory (Memory, recall, remember)
import LLM.LLM (renderHistoryWithRole)
import LLM.Provider (parseLLMJSON)
import LLM.Types
  ( ContentWithRole
  , ConvoAnswer (..)
  , ConvoError (..)
  , ConvoQuery (..)
  , ConvoQuestion (..)
  , GPTRole (System)
  , RelevantContext
  , Tag
  , cwr
  )
import Scrappy.JSON.Value (FromJValue)

-- | Semantic errors are post-parse failures: the output is well-typed (it
-- parses) but violates invariants the downstream consumer assumes. Each consumer
-- defines its own error ADT + a validator and implements this class; the
-- 'askValidated' loop uses 'remediation' to feed fix guidance into the retry.
class (Eq e, Ord e, Show e) => SemanticError e where
  -- | Human-readable description (for logging).
  describeError :: e -> String
  -- | LLM-facing fix guidance, injected into the next attempt's prompt.
  remediation :: e -> T.Text
  -- | After @n@ repetitions of this exact error, is our prompt (not the model)
  -- the problem? Default: 3 repetitions ⇒ fatal.
  isFatal :: Int -> e -> Bool
  isFatal n _ = n >= 3

-- | Ask provider @p@ for a JSON value with conversation context, validate it,
-- and self-repair: on semantic errors, prepend each error's 'remediation' to the
-- next attempt's prompt and retry, up to @maxAttempts@ times. Parses via the
-- library's scrape-based 'parseLLMJSON'. Records each parsed exchange in 'Memory'
-- (so a later 'recall' can surface the model's own prior, rejected answer), while
-- also carrying the feedback in-loop so the correction is seen regardless of how
-- the 'RelevantContext' selects history.
askValidated
  :: forall p a e es. (LLM p :> es, Memory :> es, FromJValue a, SemanticError e)
  => Int
  -> RelevantContext
  -> (Tag, ConvoQuestion)
  -> (a -> [e])
  -> Eff es (Either ConvoError a)
askValidated maxAttempts relCtx (tag, ConvoQuestion contents) validate =
    go maxAttempts []
  where
    go :: Int -> [ContentWithRole] -> Eff es (Either ConvoError a)
    go n feedback
      | n <= 0 = pure (Left (ConvoError "askValidated: validation attempts exhausted"))
      | otherwise = do
          hist <- recall relCtx
          let ctxMsgs = if null hist then [] else [renderHistoryWithRole System hist]
          res <- ask @p (ctxMsgs <> feedback <> contents)
          case res of
            Left e -> pure (Left (ConvoError e))
            Right txt -> case parseLLMJSON txt of
              Nothing -> go (n - 1) feedback
              Just a -> do
                remember (ConvoQuery tag (ConvoQuestion contents) (ConvoAnswer txt))
                case validate a of
                  [] -> pure (Right a)
                  errs -> go (n - 1) [cwr System (renderFeedback errs)]

    renderFeedback :: [e] -> T.Text
    renderFeedback errs =
      T.unlines $
        "YOUR PREVIOUS RESPONSE FAILED SEMANTIC VALIDATION. Fix these and resend the full JSON:"
          : map (\e -> "  - " <> remediation e) errs
