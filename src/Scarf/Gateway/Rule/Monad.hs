module Scarf.Gateway.Rule.Monad
  ( MonadMatch,
    runMatch,
  )
where

type MonadMatch m = m ~ IO

-- | Run a MonadMatch into IO.
runMatch :: (MonadMatch m) => m a -> IO a
runMatch = id
