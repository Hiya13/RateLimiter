module Main where

import Control.Concurrent.Async (mapConcurrently)
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
        , rlcWindowSize = 10
        }
      numConcurrentRequests = 50 :: Int

  -- Fire 50 requests at the SAME user, all at once, from 50 threads.
  decisions <- mapConcurrently
    (const $ do
      now <- getCurrentTime
      checkAndUpdate store uid (\maybeState -> checkFixedWindow cfg maybeState now))
    [1 .. numConcurrentRequests]

  let allowedCount = length (filter (== Allowed) decisions)
      deniedCount  = length (filter (== Denied) decisions)

  putStrLn $ "Allowed: " ++ show allowedCount
  putStrLn $ "Denied:  " ++ show deniedCount

  if allowedCount > rlcCapacity cfg
    then putStrLn "BUG: allowed more requests than capacity!"
    else putStrLn "OK: never exceeded capacity, even under concurrency."