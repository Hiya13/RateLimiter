module RateLimiter.Types
  ( UserId (..)
  , Decision (..)
  , RateLimitConfig (..)
  , FixedWindowState (..)
  , TokenBucketState (..)
  , SlidingWindowState (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

-- | Identifies a user for rate limiting purposes.
newtype UserId = UserId Text
  deriving (Eq, Ord, Show)

-- | The result of a rate limit check.
data Decision = Allowed | Denied
  deriving (Eq, Show)

-- | Configuration for a rate limiter.
data RateLimitConfig = RateLimitConfig
  { rlcCapacity   :: Int
  , rlcRefillRate :: Double
  , rlcWindowSize :: Int
  } deriving (Eq, Show)

-- | State for the Fixed Window algorithm.
data FixedWindowState = FixedWindowState
  { fwsWindowStart :: UTCTime
  , fwsCount       :: Int
  } deriving (Eq, Show)

-- | State for the Token Bucket algorithm.
data TokenBucketState = TokenBucketState
  { tbsTokens     :: Double
  , tbsLastRefill :: UTCTime
  } deriving (Eq, Show)

-- | State for the Sliding Window (log-based) algorithm: a list of
-- timestamps of recent requests within the window.
newtype SlidingWindowState = SlidingWindowState
  { swsTimestamps :: [UTCTime]
  } deriving (Eq, Show)