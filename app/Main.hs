module Main where

import Control.Monad (replicateM_)
import Data.Time (getCurrentTime)
import qualified Data.Text as T

import RateLimiter.Types
import RateLimiter.Store
import RateLimiter.Algorithm.FixedWindow (checkFixedWindow)

main :: IO ()
main = do
  store <- newStore
  let uid = UserId (T.pack "user123")
      cfg = RateLimitConfig
        { rlcCapacity   = 3
        , rlcRefillRate = 0
        , rlcWindowSize = 10  -- 10 second window
        }

  -- Fire 5 requests in a row for the same user.
  -- With capacity 3, we expect: Allowed, Allowed, Allowed, Denied, Denied
  replicateM_ 5 $ do
    now <- getCurrentTime
    decision <- checkAndUpdate store uid (\maybeState -> checkFixedWindow cfg maybeState now)
    print decision