module RateLimiter.Store
  ( Store
  , newStore
  , checkAndUpdate
  ) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import RateLimiter.Types (UserId)

-- | A generic, thread-safe store mapping users to their algorithm state.
-- The type parameter 's' is the per-algorithm state type
-- (e.g. FixedWindowState, and later TokenBucketState, SlidingWindowState).
newtype Store s = Store (TVar (Map.Map UserId s))

-- | Create a new, empty store.
newStore :: IO (Store s)
newStore = Store <$> newTVarIO Map.empty

-- | Atomically check-and-update a user's state using a pure transition
-- function. The transition function receives the user's current state
-- (Nothing if they've never been seen before) and returns a decision
-- plus the new state to persist.
--
-- This is generic over the algorithm: FixedWindow, TokenBucket, and
-- SlidingWindow will all plug into this same function with different
-- transition logic and state types.
checkAndUpdate
  :: Store s
  -> UserId
  -> (Maybe s -> (decision, s))
  -> IO decision
checkAndUpdate (Store tvar) uid transition =
  atomically $ do
    m <- readTVar tvar
    let current = Map.lookup uid m
        (decision, newState) = transition current
    writeTVar tvar (Map.insert uid newState m)
    pure decision