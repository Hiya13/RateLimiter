module RateLimiter.Types
  ( UserId (..)
  , Decision (..)
  , RateLimitConfig (..)
  , FixedWindowState (..)
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
  { rlcCapacity   :: Int     -- ^ max tokens / requests allowed
  , rlcRefillRate :: Double  -- ^ tokens added per second (token bucket)
  , rlcWindowSize :: Int     -- ^ window length in seconds (fixed/sliding window)
  } deriving (Eq, Show)

-- | State for the Fixed Window algorithm: when the current window
-- started, and how many requests have been counted in it so far.
data FixedWindowState = FixedWindowState
  { fwsWindowStart :: UTCTime
  , fwsCount       :: Int
  } deriving (Eq, Show)