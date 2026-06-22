module RateLimiter.Algorithm.TokenBucket
  ( checkTokenBucket
  ) where

import Data.Time (UTCTime, diffUTCTime)
import RateLimiter.Types

-- | Pure decision logic for the Token Bucket algorithm.
--
-- Given:
--   * the rate limit config (capacity, refill rate per second)
--   * the existing state for this user (if any — Nothing means first request ever)
--   * the current time
--
-- Returns the decision (Allowed/Denied) and the new state to store.
--
-- Logic:
--   * If there's no prior state, the bucket starts full (capacity tokens),
--     minus 1 for this request, Allowed.
--   * Otherwise:
--       - compute elapsed time since last refill
--       - refill = elapsed * refillRate, added to existing tokens, capped at capacity
--       - if refilled tokens >= 1, deduct 1, Allow
--       - otherwise Deny, but still persist the refilled token count
--         (so future requests benefit from the refill that already happened)
checkTokenBucket
  :: RateLimitConfig
  -> Maybe TokenBucketState
  -> UTCTime
  -> (Decision, TokenBucketState)
checkTokenBucket cfg Nothing now =
  ( Allowed
  , TokenBucketState
      { tbsTokens     = fromIntegral (rlcCapacity cfg) - 1
      , tbsLastRefill = now
      }
  )
checkTokenBucket cfg (Just st) now
  | refilled >= 1 =
      (Allowed, st { tbsTokens = refilled - 1, tbsLastRefill = now })
  | otherwise =
      (Denied, st { tbsTokens = refilled, tbsLastRefill = now })
  where
    elapsed   = realToFrac (diffUTCTime now (tbsLastRefill st)) :: Double
    refillAmt = elapsed * rlcRefillRate cfg
    refilled  = min (fromIntegral (rlcCapacity cfg)) (tbsTokens st + refillAmt)