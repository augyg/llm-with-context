{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | A narrow 'Clock' effect: read the wall clock without giving the consumer
-- the whole 'IOE' row. The default interpreter calls 'Data.Time.getCurrentTime'
-- through 'unsafeEff_' so the public signature stays @Eff (Clock : es) a ->
-- Eff es a@ — no 'IOE' leak into client code.
module LLM.Effect.Clock
  ( -- * Effect
    Clock (..)
    -- * Operations
  , getCurrentTime
    -- * Interpreter
  , runClockIO
  ) where

import qualified Data.Time.Clock as Time
import Data.Time.Clock (UTCTime)

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Internal.Monad (unsafeEff_)

data Clock :: Effect where
  GetCurrentTime :: Clock m UTCTime

type instance DispatchOf Clock = Dynamic

getCurrentTime :: (Clock :> es) => Eff es UTCTime
getCurrentTime = send GetCurrentTime

-- | Real-clock interpreter. Uses 'unsafeEff_' rather than an @IOE@ constraint
-- so consumer code never has to grow an @IOE@ row just to read the time.
runClockIO :: Eff (Clock : es) a -> Eff es a
runClockIO = interpret $ \_ -> \case
  GetCurrentTime -> unsafeEff_ Time.getCurrentTime
