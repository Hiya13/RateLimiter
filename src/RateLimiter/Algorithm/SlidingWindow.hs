module RateLimiter.Algorithm.SlidingWindow
  ( checkSlidingWindow
  ) where

import Data.Time (UTCTime, diffUTCTime)
import RateLimiter.Types

-- | Pure decision logic for the Sliding Window (log-based) algorithm.
--
-- Given:
--   * the rate limit config (capacity, window size in seconds)
--   * the existing state for this user (if any)
--   * the current time
--
-- Returns the decision and the new state.
--
-- Logic:
--   * Prune any timestamps older than (now - windowSize) from the log.
--   * If the pruned count < capacity, allow and append `now` to the log.
--   * Otherwise deny, keeping the pruned (but not appended) log.
checkSlidingWindow
  :: RateLimitConfig
  -> Maybe SlidingWindowState
  -> UTCTime
  -> (Decision, SlidingWindowState)
checkSlidingWindow cfg maybeState now =
  if length recent < rlcCapacity cfg
    then (Allowed, SlidingWindowState (now : recent))
    else (Denied,  SlidingWindowState recent)
  where
    existing = maybe [] swsTimestamps maybeState
    windowSeconds = fromIntegral (rlcWindowSize cfg)
    recent = filter (\t -> diffUTCTime now t <= windowSeconds) existing