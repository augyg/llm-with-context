{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | A pre-call token budget for LLM consumers. The library exposes the
-- 'Budget' effect, an interpreter that requires 'Clock' + 'FileSystem' +
-- 'Atomic BudgetState' in the consumer's row, and a 'loadOrInit' helper.
-- The consumer is responsible for composing the narrow IO effects (typically:
-- 'runClockIO' . 'runFileSystemIO' . 'runAtomicIO' on the loaded initial
-- state). This keeps the lib's surface narrow and lets the consumer choose
-- where the IO actually lands.
--
-- See 'runBudget' for the full assembly recipe in haddock; the consumer's
-- entry point (e.g. an application's @execPipeline@) usually wraps the
-- assembly in a single 'withBudget' combinator.
module LLM.Effect.Budget
  ( -- * Effect
    Budget (..)
  , BudgetState (..)
  , BudgetConfig (..)
  , BudgetExhausted (..)
    -- * Operations
  , checkAndDebit
  , debit
  , peekBudget
  , forceReset
    -- * Charge-for-call helpers
  , chargeForCall
  , estimateInputTokens
    -- * Interpreter
  , runBudget
  , runBudgetIO
  , loadOrInit
    -- * Config loading
  , loadBudgetConfigOrDefault
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import GHC.Generics (Generic)
import qualified System.Directory as Dir

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, inject, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)

import LLM.Effect.Clock (Clock, getCurrentTime, runClockIO)
import LLM.Effect.Concurrent (Atomic, runAtomicIO, withAtomic)
import LLM.Effect.FileSystem (FileSystem, atomicWriteJSONFile, readJSONFile, runFileSystemIO)
import LLM.Effect.Memory (estimateTokens)
import LLM.Types (ContentWithRole, _cwr_content)

-- | The persisted counter state. Serialised to JSON between calls.
data BudgetState = BudgetState
  { _budgetState_windowStart :: UTCTime
  , _budgetState_spent       :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON BudgetState
instance FromJSON BudgetState

-- | Consumer-supplied configuration. The window resets when its duration has
-- elapsed since 'windowStart'; the cap is the maximum tokens allowed inside
-- one window. JSON instances are provided so a consumer can load this
-- directly from e.g. @configs/budget.json@ without re-deriving.
data BudgetConfig = BudgetConfig
  { _budgetConfig_capTokens     :: Int
  , _budgetConfig_windowSeconds :: Int
  , _budgetConfig_stateFile     :: FilePath
  } deriving (Show, Eq, Generic)

instance ToJSON BudgetConfig
instance FromJSON BudgetConfig

-- | Returned by 'checkAndDebit' when a request would exceed the cap. The
-- consumer uses the fields to render a useful error message before aborting.
data BudgetExhausted = BudgetExhausted
  { _budgetExhausted_requested :: Int
  , _budgetExhausted_remaining :: Int
  , _budgetExhausted_resetAt   :: UTCTime
  } deriving (Show, Eq)

-- | Token-bucket budget effect.
data Budget :: Effect where
  CheckAndDebit :: Int -> Budget m (Either BudgetExhausted ())
  Debit         :: Int -> Budget m ()
  PeekBudget    :: Budget m BudgetState
  ForceReset    :: Budget m ()

type instance DispatchOf Budget = Dynamic

-- | Atomically check whether @n@ tokens fit in the remaining budget; if so,
-- debit them and return @Right ()@. If not, return @Left 'BudgetExhausted'@.
-- Call this BEFORE the LLM call to enforce a pre-flight cap.
checkAndDebit :: (Budget :> es) => Int -> Eff es (Either BudgetExhausted ())
checkAndDebit n = send (CheckAndDebit n)

-- | Debit @n@ tokens unconditionally — no cap check, no failure mode. Use this
-- when the consumer's policy has already decided the call is allowed (e.g. a
-- time-of-day rule that bypasses the cap during off-hours) but spending should
-- still be tracked. Pair with 'peekBudget' to log running totals.
debit :: (Budget :> es) => Int -> Eff es ()
debit n = send (Debit n)

-- | Read the current budget state (window start + tokens spent so far).
peekBudget :: (Budget :> es) => Eff es BudgetState
peekBudget = send PeekBudget

-- | Force a new window starting now; used by tests and ops tools.
forceReset :: (Budget :> es) => Eff es ()
forceReset = send ForceReset

-- | Estimate input tokens for a message list using the same chars/4 heuristic
-- as 'LLM.Effect.Memory.estimateTokens'. Sums the estimate across every
-- 'ContentWithRole' in the list.
estimateInputTokens :: [ContentWithRole] -> Int
estimateInputTokens = sum . map (estimateTokens . _cwr_content)

-- | Pre-call gate one combined cost: estimate input tokens from the message
-- list, add the caller's max-output estimate, atomically check-and-debit. The
-- returned 'Int' is the total estimate that was checked, so consumers who
-- want to log it don't have to recompute 'estimateInputTokens' a second time.
chargeForCall
  :: (Budget :> es)
  => Int                  -- ^ max output tokens estimate
  -> [ContentWithRole]    -- ^ input messages (used for input estimate)
  -> Eff es (Int, Either BudgetExhausted ())
chargeForCall maxOut msgs = do
  let estTotal = estimateInputTokens msgs + maxOut
  outcome <- checkAndDebit estTotal
  pure (estTotal, outcome)

-- | Interpret 'Budget' using 'Clock' for the wall clock, 'FileSystem' for
-- state persistence, and 'Atomic BudgetState' for the in-process counter.
-- The caller is responsible for setting up those three effects — typically:
--
-- > runClockIO . runFileSystemIO $ do
-- >   initial <- loadOrInit (_budgetConfig_stateFile cfg)
-- >   runAtomicIO initial . runBudget cfg $ action
--
-- The window auto-resets when @now - windowStart > windowSeconds@.
runBudget
  :: ( Clock :> es
     , FileSystem :> es
     , Atomic BudgetState :> es
     )
  => BudgetConfig
  -> Eff (Budget : es) a
  -> Eff es a
runBudget cfg = interpret $ \_ -> \case
  CheckAndDebit n -> withAtomic $ \s -> do
    now <- getCurrentTime
    let s'      = maybeReset cfg now s
        remain  = _budgetConfig_capTokens cfg - _budgetState_spent s'
        resetAt = addUTCTime
                    (fromIntegral (_budgetConfig_windowSeconds cfg))
                    (_budgetState_windowStart s')
    if n > remain
      then pure (s', Left (BudgetExhausted n remain resetAt))
      else do
        let s'' = s' { _budgetState_spent = _budgetState_spent s' + n }
        atomicWriteJSONFile (_budgetConfig_stateFile cfg) s''
        pure (s'', Right ())
  Debit n -> withAtomic $ \s -> do
    now <- getCurrentTime
    let s' = maybeReset cfg now s
        s'' = s' { _budgetState_spent = _budgetState_spent s' + n }
    atomicWriteJSONFile (_budgetConfig_stateFile cfg) s''
    pure (s'', ())
  PeekBudget -> withAtomic (\s -> pure (s, s))
  ForceReset -> withAtomic $ \_ -> do
    now <- getCurrentTime
    let fresh = BudgetState now 0
    atomicWriteJSONFile (_budgetConfig_stateFile cfg) fresh
    pure (fresh, ())

-- | Reset the window if its duration has elapsed since 'windowStart'.
maybeReset :: BudgetConfig -> UTCTime -> BudgetState -> BudgetState
maybeReset cfg now s
  | diffUTCTime now (_budgetState_windowStart s)
      > fromIntegral (_budgetConfig_windowSeconds cfg)
  = BudgetState now 0
  | otherwise = s

-- | Load the persisted state, or initialise a fresh window if the file is
-- missing or corrupt. The 'Clock' is used to stamp a fresh window's start
-- time; 'FileSystem' to read the JSON file.
loadOrInit
  :: (Clock :> es, FileSystem :> es)
  => FilePath -> Eff es BudgetState
loadOrInit path = do
  decoded <- readJSONFile path
  case decoded of
    Right s -> pure s
    Left _  -> do
      now <- getCurrentTime
      pure (BudgetState now 0)

-- | All-in-one IO-grounded interpreter that composes the three narrow
-- interpreters ('runClockIO' + 'runFileSystemIO' + 'runAtomicIO') under
-- the public 'Budget' effect, leaving only @Budget@ visible to the inner
-- action. Use this when the consumer doesn't want to hand-compose the
-- assembly recipe shown in 'runBudget'\'s haddock — i.e. most apps.
--
-- > runBudgetIO cfg $ do
-- >    requireTokens 64 msgs
-- >    ask msgs
--
-- The state file (@_budgetConfig_stateFile cfg@) is read at start and
-- rewritten atomically on every debit / reset.
runBudgetIO
  :: (IOE :> es)
  => BudgetConfig
  -> Eff (Budget : es) a
  -> Eff es a
runBudgetIO cfg act =
  runClockIO . runFileSystemIO $ do
    initial <- loadOrInit (_budgetConfig_stateFile cfg)
    runAtomicIO initial (runBudget cfg (inject act))

-- | Load a 'BudgetConfig' from a JSON file, falling back to @fallback@
-- when the file is missing or unparseable. The fallback lets each app
-- bake in its own defaults (cap / window / state-file path) without the
-- library taking an opinion.
--
-- Pure 'IO' rather than effectful so it can run before the effect row
-- is set up — typical pattern is @loadBudgetConfigOrDefault path
-- myDefaults@ inside @main@ before 'runBudgetIO'.
loadBudgetConfigOrDefault
  :: FilePath        -- ^ JSON config path (e.g. @configs/budget.json@)
  -> BudgetConfig    -- ^ fallback if the file is missing or unparseable
  -> IO BudgetConfig
loadBudgetConfigOrDefault path fallback = do
  exists <- Dir.doesFileExist path
  if not exists
    then pure fallback
    else do
      bs <- BL.readFile path
      case Aeson.eitherDecode' bs of
        Right c -> pure c
        Left _  -> pure fallback
