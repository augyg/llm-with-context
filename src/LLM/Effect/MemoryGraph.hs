{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @MemoryGraph@ — graph-structured, EDITABLE conversation history. Same shape
-- as "LLM.Effect.BrainGraph" but mutable during the session: each turn is a
-- node keyed by an auto-incrementing message number ('MsgId'), so linear order
-- is recoverable from the key ('recallRecent'), while relational edges let you
-- pull *relevant* context by graph traversal ('recallAround').
--
-- Because 'MsgId' is 'Int' — exactly fgl's node id — turns ARE fgl nodes
-- directly, so (unlike BrainGraph) no NodeId↔Int mapping is needed.
module LLM.Effect.MemoryGraph
  ( -- * Effect
    MemoryGraph (..)
  , MsgId
  , MemoryGraphState (..)
  , emptyMemoryGraph
    -- * Operations
  , addTurn
  , relateTurns
  , getTurn
  , turnNeighbors
  , recallRecent
  , recallAround
  , removeTurn
    -- * Interpreter
  , runMemoryGraphState
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Data.Graph.Inductive.Graph (delNode, empty, insEdge, insNode, lsuc)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.BFS (level)

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Local (State, gets, modify, state)

-- | A turn's message number — also its fgl node id (order = numeric order).
type MsgId = Int

data MemoryGraphState = MemoryGraphState
  { _mgs_payloads :: Map MsgId T.Text
  , _mgs_graph :: Gr () T.Text
  , _mgs_next :: MsgId
  }

emptyMemoryGraph :: MemoryGraphState
emptyMemoryGraph = MemoryGraphState Map.empty empty 0

data MemoryGraph :: Effect where
  -- | Append a turn; returns its (sequential) message number.
  AddTurn :: T.Text -> MemoryGraph m MsgId
  -- | Add a directed labeled edge between two turns.
  RelateTurns :: MsgId -> T.Text -> MsgId -> MemoryGraph m ()
  GetTurn :: MsgId -> MemoryGraph m (Maybe T.Text)
  -- | Outgoing labeled edges of a turn: @(label, target)@.
  TurnNeighbors :: MsgId -> MemoryGraph m [(T.Text, MsgId)]
  -- | The most recent @n@ turns, newest first, in conversation order.
  RecallRecent :: Int -> MemoryGraph m [(MsgId, T.Text)]
  -- | Render the @depth@-hop neighborhood of a turn as text (relevant context).
  RecallAround :: MsgId -> Int -> MemoryGraph m T.Text
  RemoveTurn :: MsgId -> MemoryGraph m ()

type instance DispatchOf MemoryGraph = Dynamic

addTurn :: (MemoryGraph :> es) => T.Text -> Eff es MsgId
addTurn = send . AddTurn

relateTurns :: (MemoryGraph :> es) => MsgId -> T.Text -> MsgId -> Eff es ()
relateTurns from label to = send (RelateTurns from label to)

getTurn :: (MemoryGraph :> es) => MsgId -> Eff es (Maybe T.Text)
getTurn = send . GetTurn

turnNeighbors :: (MemoryGraph :> es) => MsgId -> Eff es [(T.Text, MsgId)]
turnNeighbors = send . TurnNeighbors

recallRecent :: (MemoryGraph :> es) => Int -> Eff es [(MsgId, T.Text)]
recallRecent = send . RecallRecent

recallAround :: (MemoryGraph :> es) => MsgId -> Int -> Eff es T.Text
recallAround mid depth = send (RecallAround mid depth)

removeTurn :: (MemoryGraph :> es) => MsgId -> Eff es ()
removeTurn = send . RemoveTurn

-- | Default in-memory backend over effectful 'State'.
runMemoryGraphState :: (State MemoryGraphState :> es) => Eff (MemoryGraph : es) a -> Eff es a
runMemoryGraphState = interpret $ \_ -> \case
  AddTurn payload -> state $ \s ->
    let mid = _mgs_next s
        s' = s
          { _mgs_payloads = Map.insert mid payload (_mgs_payloads s)
          , _mgs_graph = insNode (mid, ()) (_mgs_graph s)
          , _mgs_next = mid + 1
          }
    in (mid, s')
  RelateTurns from label to ->
    modify (\s -> s { _mgs_graph = insEdge (from, to, label) (_mgs_graph s) })
  GetTurn mid -> gets (Map.lookup mid . _mgs_payloads)
  TurnNeighbors mid -> gets (`outgoing` mid)
  RecallRecent n -> gets (take n . Map.toDescList . _mgs_payloads)
  RecallAround mid depth -> gets (renderNeighborhood mid depth)
  RemoveTurn mid -> modify $ \s -> s
    { _mgs_graph = delNode mid (_mgs_graph s)
    , _mgs_payloads = Map.delete mid (_mgs_payloads s)
    }

outgoing :: MemoryGraphState -> MsgId -> [(T.Text, MsgId)]
outgoing s mid = [(label, j) | (j, label) <- lsuc (_mgs_graph s) mid]

renderNeighborhood :: MsgId -> Int -> MemoryGraphState -> T.Text
renderNeighborhood mid depth s =
  let within = [i | (i, d) <- level mid (_mgs_graph s), d <= depth]
  in T.intercalate "\n" (map renderTurn within)
  where
    renderTurn i =
      let payload = fromMaybe "" (Map.lookup i (_mgs_payloads s))
          edges = ["  -[" <> label <> "]-> " <> T.pack (show j) | (label, j) <- outgoing s i]
      in T.intercalate "\n" ((T.pack (show i) <> ": " <> payload) : edges)
