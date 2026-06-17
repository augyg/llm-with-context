{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | A narrow 'Atomic' effect: one consumer-scoped atomic cell parameterised
-- on its state type. The 'MVar'-backed interpreter is allocated by
-- 'runAtomicIO' and guards the cell for the lifetime of the interpreted
-- action — enough for the Budget interpreter to atomically read-check-write
-- the counter without leaking 'IOE' to consumers.
module LLM.Effect.Concurrent
  ( -- * Effect
    Atomic (..)
    -- * Operations
  , withAtomic
  , readAtomic
    -- * Interpreter
  , runAtomicIO
  ) where

import Control.Concurrent.MVar (modifyMVar, newMVar)

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.Internal.Monad (unsafeEff_)

-- | Effect parameterised on the cell's state type @s@. One @'Atomic' s@ row
-- corresponds to exactly one cell; stack multiple rows to get multiple cells.
data Atomic s :: Effect where
  WithAtomic :: (s -> m (s, a)) -> Atomic s m a

type instance DispatchOf (Atomic s) = Dynamic

-- | Atomically read-modify-write the cell. The transition runs while the
-- underlying 'MVar' is held; other consumers block.
withAtomic :: (Atomic s :> es) => (s -> Eff es (s, a)) -> Eff es a
withAtomic f = send (WithAtomic f)

-- | Atomically read the cell.
readAtomic :: (Atomic s :> es) => Eff es s
readAtomic = withAtomic (\s -> pure (s, s))

-- | 'MVar'-backed interpreter. Requires 'IOE' in the consumer row only because
-- 'localSeqUnliftIO' (used to unlift the user-supplied transition into IO
-- inside 'modifyMVar') is gated on it; pure-transition consumers could be
-- written without IOE if a stricter API is added later. The Budget interpreter
-- intentionally /does not/ require IOE — its consumer composes 'runAtomicIO'
-- separately, isolating the IOE constraint to a single layer.
runAtomicIO :: (IOE :> es) => s -> Eff (Atomic s : es) a -> Eff es a
runAtomicIO s0 act = do
  mv <- unsafeEff_ (newMVar s0)
  interpret (handler mv) act
 where
  handler mv env = \case
    WithAtomic f ->
      localSeqUnliftIO env $ \unlift ->
        modifyMVar mv (unlift . f)
