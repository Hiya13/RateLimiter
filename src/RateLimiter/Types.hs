{-# LANGUAGE DeriveGeneric #-}

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
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Identifies a user for rate limiting purposes.
newtype UserId = UserId Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserId
instance FromJSON UserId

-- | The result of a rate limit check.
data Decision = Allowed | Denied
  deriving (Eq, Show, Generic)

instance ToJSON Decision
instance FromJSON Decision

-- | Configuration for a rate limiter.
data RateLimitConfig = RateLimitConfig
  { rlcCapacity   :: Int
  , rlcRefillRate :: Double
  , rlcWindowSize :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON RateLimitConfig
instance FromJSON RateLimitConfig

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

-- | State for the Sliding Window (log-based) algorithm.
newtype SlidingWindowState = SlidingWindowState
  { swsTimestamps :: [UTCTime]
  } deriving (Eq, Show)