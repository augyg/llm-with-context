{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @Brain@ — a writable, symbolically-keyed knowledge base: the keyed-memory
-- core of the BrainGraph design.
--
-- Each entry is a 'BrainKey' (its noun\/verb\/adjective lemma sets) pointing at a
-- markdown value (a @claude.md@-style @##@ chunk), with @[[links]]@ to other
-- entries modelled as graph edges. Retrieval is deterministic and LLM-free:
--
--   1. candidate generation via an inverted index (entries sharing ≥1 term);
--   2. score each candidate by weighted per-bucket Jaccard overlap with the
--      query key — nouns dominate, verbs\/adjectives count for little
--      ('KeyWeights'); empty-on-both-sides buckets are ignored, so a perfect
--      noun match still scores 1.0;
--   3. keep candidates at or above the effort threshold (the seeds);
--   4. spread activation @reHops@ steps out along @[[links]]@.
--
-- The graph substrate is fgl, used exactly as "LLM.Effect.MemoryGraph" uses it
-- ('BrainId' /is/ the fgl node id, 'level' gives the BFS neighbourhood). Like
-- 'LLM.Effect.Memory.Memory', the effect is storage-agnostic: 'runBrainState' is
-- the in-memory default, and a beam-postgres interpreter can drop in unchanged.
module LLM.Brain.Store
  ( -- * Effect
    Brain (..)
  , BrainId
  , BrainEntry (..)
    -- * Retrieval tuning
  , RecallEffort (..)
  , defaultEffort
  , KeyWeights (..)
  , defaultWeights
    -- * Operations
  , rememberKeyed
  , getEntry
  , linkEntries
  , entryNeighbors
  , allEntries
  , recallByKey
  , collisions
  , replaceEntries
    -- * Pure merge combinator (Concat strategy)
  , compactConcat
    -- * Scoring (pure; exported for tuning + tests)
  , overlapScore
  , jaccard
    -- * Default in-memory interpreter
  , BrainState (..)
  , emptyBrain
  , runBrainState
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Data.Graph.Inductive.Graph (delNode, empty, insEdge, insNode, lpre, lsuc)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.BFS (level)

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Local (State, gets, modify, state)

import LLM.Brain.Key (BrainKey (..), keyTerms, renderBrainKey)

-- | An entry's id — also its fgl node id, so entries ARE graph nodes directly
-- (no id↔node translation needed, exactly like 'LLM.Effect.MemoryGraph.MsgId').
type BrainId = Int

-- | One keyed memory: its symbolic key, its markdown value, and a logical
-- insertion time (a monotonic counter — larger is newer) for recency ordering.
data BrainEntry = BrainEntry
  { beId    :: BrainId
  , beKey   :: BrainKey
  , beValue :: T.Text
  , beTime  :: Integer
  } deriving (Eq, Show)

-- | The scalar "effort" knob: how hard to work to recall. Higher effort = lower
-- 'reThreshold' (admit weaker matches), more 'reHops' (wander further along
-- links), larger 'reMaxSeeds'.
data RecallEffort = RecallEffort
  { reThreshold :: Double -- ^ minimum overlap score (0..1) for a seed
  , reHops      :: Int    -- ^ spreading-activation depth along @[[links]]@
  , reMaxSeeds  :: Int    -- ^ cap on seed entries before spreading
  } deriving (Eq, Show)

-- | A sensible middle effort: admit modest overlap, take one hop of links.
defaultEffort :: RecallEffort
defaultEffort = RecallEffort { reThreshold = 0.15, reHops = 1, reMaxSeeds = 8 }

-- | Per-bucket weights for 'overlapScore'. Nouns dominate; verbs and adjectives
-- contribute little — the experiments showed primary topics separate on nouns
-- while shared verbs\/adjectives mostly add noise.
data KeyWeights = KeyWeights
  { kwNoun :: Double
  , kwVerb :: Double
  , kwAdj  :: Double
  } deriving (Eq, Show)

-- | Noun-dominant defaults (1.0 \/ 0.3 \/ 0.2).
defaultWeights :: KeyWeights
defaultWeights = KeyWeights { kwNoun = 1.0, kwVerb = 0.3, kwAdj = 0.2 }

-- | The writable keyed-memory effect.
data Brain :: Effect where
  -- | Store a keyed entry; returns its fresh id. Identical-key entries are NOT
  -- merged here — that is 'compactConcat' \/ the LLM-summarise compaction's job.
  RememberKeyed :: BrainKey -> T.Text -> Brain m BrainId
  GetEntry :: BrainId -> Brain m (Maybe BrainEntry)
  -- | Add a directed labelled @[[link]]@ edge between two entries.
  LinkEntries :: BrainId -> T.Text -> BrainId -> Brain m ()
  -- | Outgoing links of an entry: @(label, target)@.
  EntryNeighbors :: BrainId -> Brain m [(T.Text, BrainId)]
  AllEntries :: Brain m [BrainEntry]
  -- | Deterministic recall: seeds by weighted overlap with the key (≥ threshold,
  -- capped, best first), then spreading activation along links. Seeds first
  -- (score then recency), spread-only entries after (recency).
  RecallByKey :: BrainKey -> RecallEffort -> Brain m [BrainEntry]
  -- | Groups of entries that share an identical canonical key (size > 1), keyed
  -- by the rendered key. The merge candidates.
  Collisions :: Brain m [(T.Text, [BrainId])]
  -- | Merge: delete the given entries, insert one new entry (re-keyed + new
  -- value), and rewire every external link that touched a deleted entry onto the
  -- new one. Returns the new id.
  ReplaceEntries :: [BrainId] -> BrainKey -> T.Text -> Brain m BrainId

type instance DispatchOf Brain = Dynamic

rememberKeyed :: (Brain :> es) => BrainKey -> T.Text -> Eff es BrainId
rememberKeyed k v = send (RememberKeyed k v)

getEntry :: (Brain :> es) => BrainId -> Eff es (Maybe BrainEntry)
getEntry = send . GetEntry

linkEntries :: (Brain :> es) => BrainId -> T.Text -> BrainId -> Eff es ()
linkEntries from label to = send (LinkEntries from label to)

entryNeighbors :: (Brain :> es) => BrainId -> Eff es [(T.Text, BrainId)]
entryNeighbors = send . EntryNeighbors

allEntries :: (Brain :> es) => Eff es [BrainEntry]
allEntries = send AllEntries

recallByKey :: (Brain :> es) => BrainKey -> RecallEffort -> Eff es [BrainEntry]
recallByKey k effort = send (RecallByKey k effort)

collisions :: (Brain :> es) => Eff es [(T.Text, [BrainId])]
collisions = send Collisions

replaceEntries :: (Brain :> es) => [BrainId] -> BrainKey -> T.Text -> Eff es BrainId
replaceEntries ids k v = send (ReplaceEntries ids k v)

-- | Merge every collision group by concatenating the entries' values (the
-- 'Concat' merge strategy — pure, no LLM). Returns the number of groups merged.
-- For the 'LLMSummarize' strategy see "LLM.Brain.Catalogue".
compactConcat :: (Brain :> es) => Eff es Int
compactConcat = do
  groups <- collisions
  mapM_ mergeGroup groups
  pure (length groups)
  where
    mergeGroup (_, ids) = do
      maybes <- mapM getEntry ids
      case [e | Just e <- maybes] of
        []           -> pure ()
        (firstE : _) -> do
          let merged = T.intercalate "\n\n" [beValue e | Just e <- maybes]
          _ <- replaceEntries ids (beKey firstE) merged
          pure ()

-- Scoring -------------------------------------------------------------------

-- | Jaccard similarity of two term lists: @|∩| / |∪|@ (0 when both are empty).
jaccard :: Ord a => [a] -> [a] -> Double
jaccard xs ys =
  let sx = Set.fromList xs
      sy = Set.fromList ys
      i = Set.size (Set.intersection sx sy)
      u = Set.size (Set.union sx sy)
  in if u == 0 then 0 else fromIntegral i / fromIntegral u

-- | Weighted overlap of a query key against an entry key: a weighted average of
-- per-bucket Jaccards, but only over buckets that are non-empty on at least one
-- side — so a perfect noun-only match scores 1.0 rather than being diluted by
-- empty verb\/adjective buckets. Result in @[0,1]@.
overlapScore :: KeyWeights -> BrainKey -> BrainKey -> Double
overlapScore w q e =
  let buckets =
        [ (kwNoun w, bkNouns q, bkNouns e)
        , (kwVerb w, bkVerbs q, bkVerbs e)
        , (kwAdj  w, bkAdjs  q, bkAdjs  e)
        ]
      active = [ (wt, jaccard qs xs) | (wt, qs, xs) <- buckets, not (null qs && null xs) ]
      num = sum [ wt * j | (wt, j) <- active ]
      den = sum [ wt | (wt, _) <- active ]
  in if den == 0 then 0 else num / den

-- Interpreter ---------------------------------------------------------------

-- | In-memory backing store: entries by id, the @[[link]]@ graph, an inverted
-- index (term → entries) for candidate generation, the next id, and the logical
-- clock.
data BrainState = BrainState
  { bsEntries  :: Map BrainId BrainEntry
  , bsGraph    :: Gr () T.Text
  , bsInverted :: Map T.Text (Set BrainId)
  , bsNext     :: BrainId
  , bsClock    :: Integer
  }

emptyBrain :: BrainState
emptyBrain = BrainState Map.empty empty Map.empty 0 0

-- | The default in-memory backend over effectful 'State', closing over the
-- (globally-decided) noun-dominant 'KeyWeights'. Swap this interpreter for a
-- beam-postgres one without touching callers.
runBrainState
  :: (State BrainState :> es)
  => KeyWeights
  -> Eff (Brain : es) a
  -> Eff es a
runBrainState weights = interpret $ \_ -> \case
  RememberKeyed key value -> state $ \st ->
    let i = bsNext st
        entry = BrainEntry i key value (bsClock st)
        st' = st
          { bsEntries = Map.insert i entry (bsEntries st)
          , bsGraph = insNode (i, ()) (bsGraph st)
          , bsInverted = addTerms i (keyTerms key) (bsInverted st)
          , bsNext = i + 1
          , bsClock = bsClock st + 1
          }
    in (i, st')

  GetEntry i -> gets (Map.lookup i . bsEntries)

  LinkEntries from label to ->
    modify (\st -> st { bsGraph = insEdge (from, to, label) (bsGraph st) })

  EntryNeighbors i ->
    gets (\st -> [(label, j) | (j, label) <- lsuc (bsGraph st) i])

  AllEntries -> gets (Map.elems . bsEntries)

  RecallByKey qkey effort -> gets (recallPure weights qkey effort)

  Collisions -> gets $ \st ->
    let grouped =
          Map.fromListWith (++)
            [ (renderBrainKey (beKey e), [beId e]) | e <- Map.elems (bsEntries st) ]
    in [ (k, sortBy compare ids) | (k, ids) <- Map.toList grouped, length ids > 1 ]

  ReplaceEntries ids newKey newValue -> state $ \st ->
    let i = bsNext st
        entry = BrainEntry i newKey newValue (bsClock st)
        idSet = Set.fromList ids
        g0 = bsGraph st
        -- rewire external edges (those not wholly inside the merged group) onto i
        outEdges = [ (i, tgt, label) | d <- ids, (tgt, label) <- lsuc g0 d, not (Set.member tgt idSet) ]
        inEdges  = [ (src, i, label) | d <- ids, (src, label) <- lpre g0 d, not (Set.member src idSet) ]
        gWired =
          foldr insEdge (insNode (i, ()) (foldr delNode g0 ids)) (outEdges ++ inEdges)
        invDropped = Map.map (`Set.difference` idSet) (bsInverted st)
        st' = st
          { bsEntries = Map.insert i entry (foldr Map.delete (bsEntries st) ids)
          , bsGraph = gWired
          , bsInverted = addTerms i (keyTerms newKey) invDropped
          , bsNext = i + 1
          , bsClock = bsClock st + 1
          }
    in (i, st')

-- | Register an entry id under each of its key terms in the inverted index.
addTerms :: BrainId -> [T.Text] -> Map T.Text (Set BrainId) -> Map T.Text (Set BrainId)
addTerms i terms inv = foldr (\t -> Map.insertWith Set.union t (Set.singleton i)) inv terms

-- | The pure retrieval computation behind 'RecallByKey'.
recallPure :: KeyWeights -> BrainKey -> RecallEffort -> BrainState -> [BrainEntry]
recallPure weights qkey effort st =
  let scored =
        [ (overlapScore weights qkey (beKey e), e) | e <- candidates st qkey ]
      seeds =
        take (reMaxSeeds effort)
          . sortBy (comparing (\(s, e) -> (Down s, Down (beTime e))))
          $ filter ((>= reThreshold effort) . fst) scored
      seedIds = map (beId . snd) seeds
      seedSet = Set.fromList seedIds
      spreadIds = spread st seedIds (reHops effort)
      spreadOnly =
        sortBy (comparing (Down . beTime))
          [ e | j <- spreadIds, not (Set.member j seedSet), Just e <- [Map.lookup j (bsEntries st)] ]
  in map snd seeds ++ spreadOnly

-- | Entries sharing at least one term with the query key (inverted-index lookup).
candidates :: BrainState -> BrainKey -> [BrainEntry]
candidates st qkey =
  let ids = Set.unions [ Map.findWithDefault Set.empty t (bsInverted st) | t <- keyTerms qkey ]
  in [ e | i <- Set.toList ids, Just e <- [Map.lookup i (bsEntries st)] ]

-- | The set of entry ids reachable within @hops@ steps of any seed (seeds
-- included, at distance 0).
spread :: BrainState -> [BrainId] -> Int -> [BrainId]
spread st seedIds hops =
  Set.toList . Set.unions $
    [ Set.fromList [ n | (n, d) <- level s (bsGraph st), d <= hops ] | s <- seedIds ]
