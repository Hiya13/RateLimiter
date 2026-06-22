module RateLimiter.Types
  ( UserId (..)
  , RateLimitConfig (..)
  , Decision (..)
  ) where

import Data.Text (Text)

-- | Identifies a user for rate limiting purposes.
newtype UserId = UserId Text
  deriving (Eq, Ord, Show)

-- | Configuration for a rate limit, shared across all algorithms.
-- Not every field is used by every algorithm:
--   - Fixed Window uses 'capacity' and 'windowSeconds'
--   - Token Bucket uses 'capacity' and 'refillRate'
--   - Sliding Window uses 'capacity' and 'windowSeconds'
data RateLimitConfig = RateLimitConfig
  { capacity      :: Int
  , windowSeconds :: Int
  , refillRate    :: Double
  } deriving (Eq, Show)

-- | The outcome of a rate limit check.
data Decision = Allowed | Denied
  deriving (Eq, Show)