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
module LLM.Effect.Mock
  ( runLLMMock
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

import qualified Data.Text as T
import Effectful (Eff, (:>))
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
