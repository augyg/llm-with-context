{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Time-of-day spend policy for long-running daemons that consume
-- LLM tokens.
--
-- The pipeline pattern this codifies: during the operator's sleep
-- window there's no human watching the meter, so the run is allowed
-- to consume as much as it needs and merely TRACK the spend.
-- During the day we enforce the configured cap so a runaway loop
-- can't drain credits silently.
--
-- The 'BudgetPolicy' is a binary discriminator + a time-of-day
-- mapping. Consumers wire it into a 'requireTokens'-style gate that
-- they call before every LLM 'ask'.
module LLM.Effect.Budget.Policy
  ( -- * Policy data
    BudgetPolicy (..)
    -- * Time-of-day mapping
  , defaultSleepWindow
  , getBudgetByTime
  , currentBudgetPolicy
    -- * Pre-call enforcement
  , requireTokens
  ) where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime
  ( TimeOfDay (..)
  , getCurrentTimeZone
  , localTimeOfDay
  , utcToLocalTime
  )
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Error.Static (Error, throwError)

import qualified LLM.Effect.Budget as B
import LLM.Types (ContentWithRole)

-- | Spend policy. 'Unlimited' tracks the debit but bypasses the cap;
-- 'HardLimit' atomically check-and-debits and throws if it would
-- exceed the cap.
data BudgetPolicy = Unlimited | HardLimit
  deriving (Show, Eq)

-- | Sleep-window default: @23:00–07:00@. Outside this window the
-- caller's configured cap applies; inside it spend is tracked but
-- not capped.
--
-- Exposed so consumers with a different sleep schedule can build
-- their own (start, end) pair and pass it to 'getBudgetByTime'.
defaultSleepWindow :: (TimeOfDay, TimeOfDay)
defaultSleepWindow = (TimeOfDay 23 0 0, TimeOfDay 7 0 0)

-- | Map a wall-clock 'TimeOfDay' (in whichever timezone the caller
-- resolved) to a spend policy, using @(sleepStart, sleepEnd)@. The
-- comparison treats the window as half-open: @[sleepStart, 24:00)@
-- ∪ @[00:00, sleepEnd)@ is unlimited, the rest is hard-limited.
getBudgetByTime
  :: (TimeOfDay, TimeOfDay)   -- ^ (sleepStart, sleepEnd)
  -> TimeOfDay                -- ^ the time to classify
  -> BudgetPolicy
getBudgetByTime (sleepStart, sleepEnd) tod
  | isAsleep tod = Unlimited
  | otherwise    = HardLimit
  where
    isAsleep t = t >= sleepStart || t < sleepEnd

-- | Resolve the current 'BudgetPolicy' from the system clock + system
-- timezone and the given sleep window. Called at every LLM call site
-- so the policy follows real time across a long-running daemon (a job
-- that crosses the sleep-window boundary mid-flight gracefully
-- switches policy).
currentBudgetPolicy :: (TimeOfDay, TimeOfDay) -> IO BudgetPolicy
currentBudgetPolicy window = do
  tz  <- getCurrentTimeZone
  now <- getCurrentTime
  pure (getBudgetByTime window (localTimeOfDay (utcToLocalTime tz now)))

-- | Pre-call gate: estimate input tokens for @msgs@, add @maxOut@,
-- and either atomically check-and-debit (HardLimit) or debit
-- unconditionally (Unlimited). On HardLimit cap exhaustion the
-- caller-supplied @toErr@ converts the lib's 'BudgetExhausted' into
-- the consumer's error type, which is then thrown via the ambient
-- @Error e@ effect.
--
-- Polymorphic in the error type so consumers don't have to share an
-- error ADT with the library.
requireTokens
  :: (B.Budget :> es, Error e :> es, IOE :> es, Show e)
  => (TimeOfDay, TimeOfDay)              -- ^ sleep window
  -> (Int -> Int -> String -> e)         -- ^ requested, remaining, resetAt -> error
  -> Int                                  -- ^ max output tokens estimate
  -> [ContentWithRole]                    -- ^ input messages
  -> Eff es ()
requireTokens window toErr maxOut msgs = do
  policy <- liftIO (currentBudgetPolicy window)
  let estTotal = B.estimateInputTokens msgs + maxOut
  case policy of
    Unlimited -> B.debit estTotal
    HardLimit -> do
      result <- B.checkAndDebit estTotal
      case result of
        Right () -> pure ()
        Left (B.BudgetExhausted req remain resetAt) ->
          throwError (toErr req remain (iso8601Show resetAt))
