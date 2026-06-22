module RateLimiter.Algorithm.FixedWindow
  ( checkFixedWindow
  ) where

import Data.Time (UTCTime, diffUTCTime)
import RateLimiter.Types

-- | Pure decision logic for the Fixed Window algorithm.
--
-- Given:
--   * the rate limit config (capacity, window size)
--   * the existing state for this user (if any — Nothing means first request ever)
--   * the current time
--
-- Returns the decision (Allowed/Denied) and the new state to store.
--
-- Logic:
--   * If there's no prior state, start a fresh window with count 1, Allowed.
--   * If the current time is still within the existing window:
--       - if count < capacity, increment and Allow
--       - otherwise Deny (state unchanged)
--   * If the current time has moved past the window, start a new window
--     with count 1, Allowed (this is the "fixed window reset" behavior).
checkFixedWindow
  :: RateLimitConfig
  -> Maybe FixedWindowState
  -> UTCTime
  -> (Decision, FixedWindowState)
checkFixedWindow _cfg Nothing now =
  (Allowed, FixedWindowState { fwsWindowStart = now, fwsCount = 1 })
checkFixedWindow cfg (Just st) now
  | windowExpired = (Allowed, FixedWindowState { fwsWindowStart = now, fwsCount = 1 })
  | fwsCount st < rlcCapacity cfg =
      (Allowed, st { fwsCount = fwsCount st + 1 })
  | otherwise =
      (Denied, st)
  where
    elapsed = diffUTCTime now (fwsWindowStart st)
    windowExpired = elapsed >= fromIntegral (rlcWindowSize cfg)