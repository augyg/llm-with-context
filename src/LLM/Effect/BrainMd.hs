{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @BrainMd@ — a read-only, "CLAUDE.md"-like guidance document. Reader-style:
-- the doc is fixed by the operator at the run site ('runBrainMd') and the agent
-- logic can only READ it (or render it as a 'System' turn). It cannot mutate
-- it — there are no write operations by design.
module LLM.Effect.BrainMd
  ( BrainMd (..)
  , BrainDoc (..)
  , readBrain
  , brainAsTurn
  , runBrainMd
  ) where

import LLM.Types (ContentWithRole, GPTRole (System), cwr)

import qualified Data.Text as T
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

newtype BrainDoc = BrainDoc { unBrainDoc :: T.Text } deriving (Show)

-- | Read-only guidance effect. (No @Set@/@Append@/@Revise@ — it's a Reader.)
data BrainMd :: Effect where
  ReadBrain :: BrainMd m T.Text
  BrainAsTurn :: BrainMd m ContentWithRole

type instance DispatchOf BrainMd = Dynamic

-- | The current guidance text.
readBrain :: (BrainMd :> es) => Eff es T.Text
readBrain = send ReadBrain

-- | The guidance rendered as a 'System' turn, ready to prepend to a prompt.
brainAsTurn :: (BrainMd :> es) => Eff es ContentWithRole
brainAsTurn = send BrainAsTurn

-- | Run with a fixed guidance doc (read-only; closed over, Reader-style).
runBrainMd :: BrainDoc -> Eff (BrainMd : es) a -> Eff es a
runBrainMd (BrainDoc doc) = interpret $ \_ -> \case
  ReadBrain -> pure doc
  BrainAsTurn -> pure (cwr System doc)
