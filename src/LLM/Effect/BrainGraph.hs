{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @BrainGraph@ — a READ-ONLY, graph-structured knowledge base. The operator
-- builds the graph up front (pure 'insertNode' / 'insertEdge' / 'buildBrainGraph'),
-- fixes it at the run site ('runBrainGraph', Reader-style, closed over), and the
-- agent can only query it ('getNode' / 'neighbors' / 'recallAround') — it cannot
-- mutate it. For an editable graph (conversation memory), see
-- "LLM.Effect.MemoryGraph".
--
-- UNVERIFIED: written ahead of compilation; fix on next build pass.
module LLM.Effect.BrainGraph
  ( -- * Effect (read-only)
    BrainGraph (..)
  , NodeId
    -- * Building the (immutable) graph
  , BrainGraphState (..)
  , emptyBrainGraph
  , insertNode
  , insertEdge
  , buildBrainGraph
    -- * Read operations
  , getNode
  , neighbors
  , recallAround
    -- * Interpreter
  , runBrainGraph
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Data.Graph.Inductive.Graph (Node, empty, insEdge, insNode, lsuc)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.BFS (level)

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

type NodeId = T.Text

-- | Node payloads live in a side map; the fgl graph carries the edge structure
-- (edge labels), with a 'NodeId'↔fgl-'Node' translation either way.
data BrainGraphState = BrainGraphState
  { _bgs_payloads :: Map NodeId T.Text
  , _bgs_graph :: Gr () T.Text
  , _bgs_toInt :: Map NodeId Node
  , _bgs_fromInt :: Map Node NodeId
  , _bgs_next :: Node
  }

emptyBrainGraph :: BrainGraphState
emptyBrainGraph = BrainGraphState Map.empty empty Map.empty Map.empty 0

-- | Pure builder: upsert a node's payload (creating the node if new).
insertNode :: NodeId -> T.Text -> BrainGraphState -> BrainGraphState
insertNode nid payload s0 =
  let (_, s1) = ensureNode nid s0
  in s1 { _bgs_payloads = Map.insert nid payload (_bgs_payloads s1) }

-- | Pure builder: add a directed labeled edge @from -[label]-> to@ (creating
-- either node if missing).
insertEdge :: NodeId -> T.Text -> NodeId -> BrainGraphState -> BrainGraphState
insertEdge from label to s0 =
  let (i, s1) = ensureNode from s0
      (j, s2) = ensureNode to s1
  in s2 { _bgs_graph = insEdge (i, j, label) (_bgs_graph s2) }

-- | Build a graph from a list of nodes and edges in one go.
buildBrainGraph :: [(NodeId, T.Text)] -> [(NodeId, T.Text, NodeId)] -> BrainGraphState
buildBrainGraph nodes edges =
  let withNodes = foldr (\(nid, p) acc -> insertNode nid p acc) emptyBrainGraph nodes
  in foldr (\(from, lbl, to) acc -> insertEdge from lbl to acc) withNodes edges

data BrainGraph :: Effect where
  GetNode :: NodeId -> BrainGraph m (Maybe T.Text)
  Neighbors :: NodeId -> BrainGraph m [(T.Text, NodeId)]
  RecallAround :: NodeId -> Int -> BrainGraph m T.Text

type instance DispatchOf BrainGraph = Dynamic

getNode :: (BrainGraph :> es) => NodeId -> Eff es (Maybe T.Text)
getNode = send . GetNode

neighbors :: (BrainGraph :> es) => NodeId -> Eff es [(T.Text, NodeId)]
neighbors = send . Neighbors

recallAround :: (BrainGraph :> es) => NodeId -> Int -> Eff es T.Text
recallAround nid depth = send (RecallAround nid depth)

-- | Run the agent's queries against a fixed, operator-supplied graph
-- (read-only; closed over, Reader-style — there is no mutation path).
runBrainGraph :: BrainGraphState -> Eff (BrainGraph : es) a -> Eff es a
runBrainGraph s = interpret $ \_ -> \case
  GetNode nid -> pure (Map.lookup nid (_bgs_payloads s))
  Neighbors nid -> pure (outgoing s nid)
  RecallAround nid depth -> pure (renderNeighborhood nid depth s)

-- Internals -----------------------------------------------------------------

ensureNode :: NodeId -> BrainGraphState -> (Node, BrainGraphState)
ensureNode nid s = case Map.lookup nid (_bgs_toInt s) of
  Just i -> (i, s)
  Nothing ->
    let i = _bgs_next s
    in ( i
       , s { _bgs_graph = insNode (i, ()) (_bgs_graph s)
           , _bgs_toInt = Map.insert nid i (_bgs_toInt s)
           , _bgs_fromInt = Map.insert i nid (_bgs_fromInt s)
           , _bgs_next = i + 1
           }
       )

outgoing :: BrainGraphState -> NodeId -> [(T.Text, NodeId)]
outgoing s nid = case Map.lookup nid (_bgs_toInt s) of
  Nothing -> []
  Just i ->
    [ (label, tgt)
    | (j, label) <- lsuc (_bgs_graph s) i
    , Just tgt <- [Map.lookup j (_bgs_fromInt s)]
    ]

renderNeighborhood :: NodeId -> Int -> BrainGraphState -> T.Text
renderNeighborhood nid depth s = case Map.lookup nid (_bgs_toInt s) of
  Nothing -> "(no such node: " <> nid <> ")"
  Just root ->
    let within = [i | (i, d) <- level root (_bgs_graph s), d <= depth]
    in T.intercalate "\n" (map renderNode within)
  where
    renderNode i = case Map.lookup i (_bgs_fromInt s) of
      Nothing -> ""
      Just nodeId ->
        let payload = fromMaybe "" (Map.lookup nodeId (_bgs_payloads s))
            edges = ["  -[" <> label <> "]-> " <> tgt | (label, tgt) <- outgoing s nodeId]
        in T.intercalate "\n" ((nodeId <> ": " <> payload) : edges)
