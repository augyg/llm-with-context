-- | A tiny retry combinator for the LLM ask+parse layer: re-run a
-- 'Maybe'-producing action until it yields a 'Just' or the attempt budget is
-- exhausted. Kept here (rather than re-implemented in each consumer) so there is
-- one retry mechanism. Effect- and IO-agnostic — works in any 'Monad'; the
-- action carries its own logging/effects.
module LLM.Retry
  ( retry
  ) where

-- | @retry n action@ runs @action@ up to @max 1 n@ times, returning the first
-- 'Just', or 'Nothing' if every attempt yields 'Nothing'.
retry :: Monad m => Int -> m (Maybe a) -> m (Maybe a)
retry n action = go (max 1 n)
  where
    go k = do
      r <- action
      case r of
        Just x -> pure (Just x)
        Nothing
          | k <= 1 -> pure Nothing
          | otherwise -> go (k - 1)
