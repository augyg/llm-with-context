{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure mock interpreter for any @LLM p@ — no network, no 'IOE'. Drive it with
-- a deterministic responder; run the whole stack with 'Effectful.runPureEff'
-- for fast tests. Typed and context operations reuse the real decoders/loops
-- ('askTypedBy', 'runCtx', 'runCtxTyped'), so only the transport is faked.
-- Pin the provider if it's ambiguous: @runLLMMock \@'AnthropicHttp respond@.
--
-- For tests that want to assert on WHAT the consumer asked (not just how
-- it reacted to the response), use 'runLLMMockRecording' — same behaviour
-- as 'runLLMMock' plus every prompt is appended to a caller-owned
-- 'IORef'. Requires 'IOE' for the ref mutation.
module LLM.Effect.Mock
  ( runLLMMock
  , runLLMMockRecording
  ) where

import LLM.Effect (LLM (..), runCtx, runCtxTyped)
import LLM.Effect.Memory (Memory)
import LLM.LLM (askTypedBy, renderHistory)
import LLM.Types
  ( Block (..)
  , ContentWithRole
  , RichMessage (..)
  , ToolTurn (..)
  , cwr
  )

import Data.IORef (IORef, atomicModifyIORef')
import qualified Data.Text as T
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)

runLLMMock
  :: forall p es b. (Memory :> es)
  => ([ContentWithRole] -> Either T.Text T.Text)
  -> Eff (LLM p : es) b
  -> Eff es b
runLLMMock respond = interpret $ \_ -> \case
  Ask contents             -> pure (respond contents)
  AskTyped contents        -> askTypedBy (pure . respond) contents
  AskWithContext rc q      -> runCtx injectHistory (pure . respond) rc q
  AskWithContextTyped rc q -> runCtxTyped injectHistory (pure . respond) rc q
  -- Multimodal: the mock backend has no notion of the per-provider
  -- 'ImageInput' carrier, so drop it and route the text turns through
  -- the same responder. Mock tests assert against the text payload.
  AskMultimodal _img contents -> pure (respond contents)
  -- Mock never requests tools, so an agent loop over it terminates at once.
  AskTools _defs msgs ->
    let contents = [cwr role t | RichMessage role bs <- msgs, BlockText t <- bs]
        answer = either id id (respond contents)
    in pure (Right (ToolTurn (Just answer) [] [BlockText answer]))
  where
    injectHistory histItems = [renderHistory histItems]

-- | Recording variant of 'runLLMMock'. Every 'LLM' call the interpreter
-- handles appends its @[ContentWithRole]@ payload to the supplied
-- 'IORef' (newest at HEAD; reverse for chronological order).
--
-- Recording semantics match 'runLLMMock''s response semantics:
--
--   * 'Ask' / 'AskTyped' / 'AskMultimodal' record the raw contents the
--     consumer sent. 'AskMultimodal' drops the image carrier just as
--     'runLLMMock' does — recording captures the text turns only.
--   * 'AskWithContext' / 'AskWithContextTyped' record each downstream
--     responder invocation (history-injected contents) at every retry
--     'runCtx' / 'runCtxTyped' performs. This is what was actually
--     handed to the "transport," which is the more useful invariant
--     for prompt-shape tests.
--   * 'AskTools' records the flattened text blocks (same shape
--     'runLLMMock' passes to the responder).
--
-- Requires 'IOE' for the 'atomicModifyIORef'' write — that is the
-- only IO this interpreter performs.
runLLMMockRecording
  :: forall p es b. (Memory :> es, IOE :> es)
  => IORef [[ContentWithRole]]
      -- ^ recording sink: each LLM call prepends its
      -- @[ContentWithRole]@. Newest at HEAD; 'reverse' for
      -- chronological order.
  -> ([ContentWithRole] -> Either T.Text T.Text)
      -- ^ canned responder, same shape as 'runLLMMock'.
  -> Eff (LLM p : es) b
  -> Eff es b
runLLMMockRecording sink respond = interpret $ \_ -> \case
  Ask contents -> do
    liftIO $ atomicModifyIORef' sink (\xs -> (contents : xs, ()))
    pure (respond contents)
  AskTyped contents -> do
    liftIO $ atomicModifyIORef' sink (\xs -> (contents : xs, ()))
    askTypedBy (pure . respond) contents
  AskWithContext rc q ->
    runCtx injectHistory
      (\cs -> do
         liftIO $ atomicModifyIORef' sink (\xs -> (cs : xs, ()))
         pure (respond cs))
      rc q
  AskWithContextTyped rc q ->
    runCtxTyped injectHistory
      (\cs -> do
         liftIO $ atomicModifyIORef' sink (\xs -> (cs : xs, ()))
         pure (respond cs))
      rc q
  AskMultimodal _img contents -> do
    liftIO $ atomicModifyIORef' sink (\xs -> (contents : xs, ()))
    pure (respond contents)
  AskTools _defs msgs -> do
    let contents = [cwr role t | RichMessage role bs <- msgs, BlockText t <- bs]
    liftIO $ atomicModifyIORef' sink (\xs -> (contents : xs, ()))
    let answer = either id id (respond contents)
    pure (Right (ToolTurn (Just answer) [] [BlockText answer]))
  where
    injectHistory histItems = [renderHistory histItems]
