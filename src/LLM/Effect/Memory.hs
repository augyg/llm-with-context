{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @Memory@ effect: LLM-domain-specific manipulation of a conversation's
-- history. Storage-only — it has NO dependency on the 'LLM.Effect.LLM' effect
-- (summarisation/compaction is a separate combinator in
-- "LLM.Effect.Compaction" that uses both). Swap the backing store by swapping
-- the interpreter; 'runMemoryState' is the default in-memory backend over
-- effectful's 'State'.
--
-- Pin semantics (the contract the reducers honour):
--   * @prune@, @trimToTokens@ — size-based reducers; NEVER drop a pinned turn.
--   * @forgetWhere@ — targeted predicate removal; also skips pinned turns
--     (unpin first to remove one). Keeps "pinned == protected" consistent.
--   * @forget@ — the nuclear option; clears ALL history (the pin set itself is
--     retained, so re-remembered turns with those tags are pinned again).
module LLM.Effect.Memory
  ( -- * Effect
    Memory (..)
  , MemoryStore (..)
  , MemoryCheckpoint
  , emptyMemoryStore
    -- * Operations
  , recall
  , recallAll
  , recallPinned
  , remember
  , forget
  , prune
  , forgetWhere
  , pin
  , unpin
  , note
  , trimToTokens
  , tokenCount
  , turnCount
  , amendLast
  , checkpoint
  , restore
    -- * Interpreter
  , runMemoryState
    -- * Pure helpers (exported for reuse / testing)
  , estimateTokens
  , historyTokens
  ) where

import LLM.LLM (selectRelevant)
import LLM.Types
  ( ContentWithRole (..)
  , ConversationHistory
  , ConvoAnswer (..)
  , ConvoQuery (..)
  , ConvoQuestion (..)
  , RelevantContext
  , Tag (..)
  )

import qualified Data.Text as T

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Local (State, gets, modify, put)

-- | The in-memory store: the turn history (newest-first, matching the existing
-- @modify ((:) ...)@ convention in "LLM.LLM") plus the set of pinned tags.
data MemoryStore = MemoryStore
  { _memoryStore_history :: ConversationHistory
  , _memoryStore_pinned :: [Tag]
  }

-- | A point-in-time snapshot of the whole store, for 'checkpoint' / 'restore'.
newtype MemoryCheckpoint = MemoryCheckpoint MemoryStore

emptyMemoryStore :: MemoryStore
emptyMemoryStore = MemoryStore [] []

-- | The Memory effect. @compact@ is deliberately NOT here — it needs the LLM
-- effect, so it lives in "LLM.Effect.Compaction" as a combinator over both.
data Memory :: Effect where
  Recall :: RelevantContext -> Memory m ConversationHistory
  RecallAll :: Memory m ConversationHistory
  RecallPinned :: Memory m ConversationHistory
  Remember :: ConvoQuery T.Text -> Memory m ()
  Forget :: Memory m ()
  Prune :: Int -> Memory m ()
  ForgetWhere :: (Tag -> Bool) -> Memory m ()
  Pin :: Tag -> Memory m ()
  Unpin :: Tag -> Memory m ()
  Note :: T.Text -> Memory m ()
  TrimToTokens :: Int -> Memory m ()
  TokenCount :: Memory m Int
  TurnCount :: Memory m Int
  AmendLast :: T.Text -> Memory m ()
  Checkpoint :: Memory m MemoryCheckpoint
  Restore :: MemoryCheckpoint -> Memory m ()

type instance DispatchOf Memory = Dynamic

-- | Recall the turns relevant to the given 'RelevantContext'.
recall :: (Memory :> es) => RelevantContext -> Eff es ConversationHistory
recall = send . Recall

-- | Recall the entire history.
recallAll :: (Memory :> es) => Eff es ConversationHistory
recallAll = send RecallAll

-- | Recall only the pinned turns.
recallPinned :: (Memory :> es) => Eff es ConversationHistory
recallPinned = send RecallPinned

-- | Append a question/answer turn to the history.
remember :: (Memory :> es) => ConvoQuery T.Text -> Eff es ()
remember = send . Remember

-- | Clear ALL history (pin set retained).
forget :: (Memory :> es) => Eff es ()
forget = send Forget

-- | Keep only the most recent @n@ turns, plus all pinned turns.
prune :: (Memory :> es) => Int -> Eff es ()
prune = send . Prune

-- | Drop turns whose 'Tag' matches the predicate — except pinned ones.
forgetWhere :: (Memory :> es) => (Tag -> Bool) -> Eff es ()
forgetWhere = send . ForgetWhere

-- | Protect a tag's turns from the reducers (prune/trimToTokens/forgetWhere).
pin :: (Memory :> es) => Tag -> Eff es ()
pin = send . Pin

-- | Remove a tag from the pinned set.
unpin :: (Memory :> es) => Tag -> Eff es ()
unpin = send . Unpin

-- | Inject a synthetic turn carrying free-form text (e.g. a fact to seed, or a
-- compaction summary). NOTE: the originally-proposed 'GPTRole' argument is
-- dropped — 'ConvoQuery' has no role field and 'LLM.LLM.renderHistory' ignores
-- role, so a role here would be a silent no-op. The note is stored as a turn
-- tagged @"note"@ with an empty question and the text as the answer.
note :: (Memory :> es) => T.Text -> Eff es ()
note = send . Note

-- | Drop oldest unpinned turns until the estimated token count is within @n@.
trimToTokens :: (Memory :> es) => Int -> Eff es ()
trimToTokens = send . TrimToTokens

-- | Estimated token count of the current history (see 'estimateTokens' —
-- a heuristic, not a real tokeniser).
tokenCount :: (Memory :> es) => Eff es Int
tokenCount = send TokenCount

-- | Number of turns currently in history.
turnCount :: (Memory :> es) => Eff es Int
turnCount = send TurnCount

-- | Rewrite the answer of the most recent turn (regenerate / correction flow).
amendLast :: (Memory :> es) => T.Text -> Eff es ()
amendLast = send . AmendLast

-- | Snapshot the whole store.
checkpoint :: (Memory :> es) => Eff es MemoryCheckpoint
checkpoint = send Checkpoint

-- | Restore a previously taken snapshot (branch / roll back).
restore :: (Memory :> es) => MemoryCheckpoint -> Eff es ()
restore = send . Restore

-- | Rough token estimate for a piece of text. Heuristic: ~4 characters per
-- token (no real tokeniser is available in this library). Floored at 1 for
-- non-empty input so a turn never estimates to zero.
estimateTokens :: T.Text -> Int
estimateTokens t
  | T.null t = 0
  | otherwise = max 1 (T.length t `div` 4)

-- | Estimated tokens of one turn (its question contents + its answer).
queryTokens :: ConvoQuery T.Text -> Int
queryTokens (ConvoQuery _ (ConvoQuestion q) (ConvoAnswer a)) =
  sum (map (estimateTokens . _cwr_content) q) + estimateTokens a

-- | Estimated tokens of an entire history.
historyTokens :: ConversationHistory -> Int
historyTokens = sum . map queryTokens

isPinned :: [Tag] -> ConvoQuery T.Text -> Bool
isPinned pinned q = _convoQuery_tag q `elem` pinned

-- | Default in-memory backend: interpret 'Memory' over effectful's 'State'.
-- The 'State' is left in @es@ for a later @evalState emptyMemoryStore@.
runMemoryState :: (State MemoryStore :> es) => Eff (Memory : es) a -> Eff es a
runMemoryState = interpret $ \_ -> \case
  Recall relCtx -> gets (selectRelevant relCtx . _memoryStore_history)
  RecallAll -> gets _memoryStore_history
  RecallPinned -> gets (\ms -> filter (isPinned (_memoryStore_pinned ms)) (_memoryStore_history ms))
  Remember q -> modify (\ms -> ms { _memoryStore_history = q : _memoryStore_history ms })
  Forget -> modify (\ms -> ms { _memoryStore_history = [] })
  Prune n -> modify (prune' n)
  ForgetWhere p -> modify (forgetWhere' p)
  Pin t -> modify (\ms ->
    ms { _memoryStore_pinned =
           if t `elem` _memoryStore_pinned ms
             then _memoryStore_pinned ms
             else t : _memoryStore_pinned ms })
  Unpin t -> modify (\ms -> ms { _memoryStore_pinned = filter (/= t) (_memoryStore_pinned ms) })
  Note txt -> modify (\ms ->
    ms { _memoryStore_history =
           ConvoQuery (Tag "note") (ConvoQuestion []) (ConvoAnswer txt) : _memoryStore_history ms })
  TrimToTokens n -> modify (trimToTokens' n)
  TokenCount -> gets (historyTokens . _memoryStore_history)
  TurnCount -> gets (length . _memoryStore_history)
  AmendLast txt -> modify (amendLast' txt)
  Checkpoint -> gets MemoryCheckpoint
  Restore (MemoryCheckpoint s) -> put s
  where
    -- keep most recent n (history is newest-first) OR pinned
    prune' n ms =
      ms { _memoryStore_history =
             [ q
             | (i, q) <- zip [0 :: Int ..] (_memoryStore_history ms)
             , i < n || isPinned (_memoryStore_pinned ms) q
             ] }
    forgetWhere' p ms =
      ms { _memoryStore_history =
             [ q
             | q <- _memoryStore_history ms
             , isPinned (_memoryStore_pinned ms) q || not (p (_convoQuery_tag q))
             ] }
    amendLast' txt ms = case _memoryStore_history ms of
      [] -> ms
      (ConvoQuery t q _ : rest) ->
        ms { _memoryStore_history = ConvoQuery t q (ConvoAnswer txt) : rest }
    -- drop oldest unpinned turns one at a time until under budget; stop if only
    -- pinned turns remain. O(n^2) but conversation histories are small.
    trimToTokens' budget ms = ms { _memoryStore_history = go (_memoryStore_history ms) }
      where
        pinned = _memoryStore_pinned ms
        go h
          | historyTokens h <= budget = h
          | otherwise = case dropOldestUnpinned h of
              Nothing -> h
              Just h' -> go h'
        dropOldestUnpinned h =
          let unpinnedIdxs = [ i | (i, q) <- zip [0 :: Int ..] h, not (isPinned pinned q) ]
          in case reverse unpinnedIdxs of
               [] -> Nothing
               (oldest : _) -> Just [ q | (j, q) <- zip [0 :: Int ..] h, j /= oldest ]
