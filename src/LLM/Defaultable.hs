{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Description: Gate real LLM calls on a boolean; substitute a baked
-- in-tree default when the gate says "no."
--
-- Three ask-flavored variants, same shape:
--
--   * 'askLLM'                — defined in "LLM.Provider". Always real.
--   * 'askLLMDefaultable'     — reads a monadic 'Bool' at call time
--                                (e.g. a config-file lookup). True →
--                                real; False → baked.
--   * 'askLLMDefaultableLocal' — takes a pure 'Bool' (expected: a
--                                caller-threaded @isLocalDev@). True →
--                                baked; False → real.
--
-- Compile-time coupling: every call site that returns a type @a@ picks
-- up the 'LLMDefaultable' @a@ constraint. GHC refuses to compile a new
-- call site whose @a@ lacks the instance — enforces "every substitutable
-- LLM output has a baked default."
--
-- This module intentionally has no Obelisk / env-var / config-file
-- dependency. The gate is passed in by the caller; the caller decides
-- the gate's source (Obelisk.Configs, env var, hard-coded, etc.).
module LLM.Defaultable
  ( LLMDefaultable (..)
  , askLLMDefaultable
  , askLLMDefaultableLocal
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import LLM.Provider
  ( LLMT
  , LLMError (..)
  , askLLM
  )
import LLM.Types (ContentWithRole)

-- | Types with a canonical in-tree default that stands in for a real
-- LLM response in dev / scrubbed-snapshot / variance-study contexts.
--
-- The expectation is that an instance body is produced by a one-shot
-- generator exec that hits the real LLM once with a canonical input
-- (e.g. your own resume PDF), captures the structured output, and
-- pretty-prints it as a Haskell record literal that's committed to the
-- consumer's source tree.
--
-- Keep instance values as RECORDS, not positional constructors — a
-- field addition on the underlying type then surfaces as a compile
-- error at the instance site, forcing regeneration (good: staleness is
-- loud, not silent).
class LLMDefaultable a where
  llmDefault :: a

-- | Config-gated. Caller supplies a monadic 'Bool' lookup; typical
-- callers wire this to 'Obelisk.Configs.getConfig' on
-- @config/backend/resumeReaderWorkerEnabled@ (or similar). The gate
-- fires at call-time, so flipping the config without restart changes
-- subsequent calls — the function doesn't cache.
--
-- Decoding failures from the real path are reported as 'LLMParseError';
-- the baked-default path never fails. Semantics match 'askLLM' plus a
-- JSON decode step.
askLLMDefaultable
  :: forall a m
   . (MonadIO m, LLMDefaultable a, Aeson.FromJSON a)
  => LLMT m Bool            -- ^ gate: 'True' → real; 'False' → baked
  -> [ContentWithRole]
  -> LLMT m (Either LLMError a)
askLLMDefaultable gateM msgs = do
  gate <- gateM
  if gate
    then do
      raw <- askLLM msgs
      pure $ raw >>= decodeOrErr @a
    else pure $ Right (llmDefault @a)

-- | 'isLocalDev'-gated. Caller passes the dev-vs-prod bit directly
-- (not a monadic lookup). On local dev: baked default. On prod: real
-- LLM. Use this when the gate semantics is "no credit burn on any
-- dev box, period" — stronger than config-file-gated.
--
-- Note the sign flip relative to 'askLLMDefaultable': here 'True' →
-- baked (because 'True' means "we're on dev"), there 'True' → real
-- (because 'True' means "the switch says allow real calls").
askLLMDefaultableLocal
  :: forall a m
   . (MonadIO m, LLMDefaultable a, Aeson.FromJSON a)
  => Bool                   -- ^ 'True' iff running on local dev
  -> [ContentWithRole]
  -> LLMT m (Either LLMError a)
askLLMDefaultableLocal isLocalDev msgs
  | isLocalDev = pure $ Right (llmDefault @a)
  | otherwise  = do
      raw <- askLLM msgs
      pure $ raw >>= decodeOrErr @a

-- Internal: wrap Aeson's parse-error path as an 'LLMParseError' so the
-- defaultable variants return a uniform 'Either LLMError a' shape.
decodeOrErr :: forall a. Aeson.FromJSON a => T.Text -> Either LLMError a
decodeOrErr responseText =
  case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 responseText)) of
    Right x  -> Right x
    Left err -> Left (LLMParseError (T.pack err))
