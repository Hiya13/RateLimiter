module Main where

import Test.Hspec
import Control.Concurrent.Async (mapConcurrently)
import Data.Time (getCurrentTime, addUTCTime)
import qualified Data.Text as T

import RateLimiter.Types
import RateLimiter.Store
import RateLimiter.Algorithm.FixedWindow (checkFixedWindow)
import RateLimiter.Algorithm.TokenBucket (checkTokenBucket)
import RateLimiter.Algorithm.SlidingWindow (checkSlidingWindow)

main :: IO ()
main = hspec $ do
  describe "Fixed Window rate limiter" $ do

    it "allows requests up to capacity, sequentially" $ do
      store <- newStore
      let uid = UserId (T.pack "alice")
          cfg = RateLimitConfig
            { rlcCapacity = 3, rlcRefillRate = 0, rlcWindowSize = 10 }

      decisions <- mapM
        (const $ do
          now <- getCurrentTime
          checkAndUpdate store uid (\s -> checkFixedWindow cfg s now))
        [1 .. 5 :: Int]

      decisions `shouldBe` [Allowed, Allowed, Allowed, Denied, Denied]

    it "never allows more than capacity, even under concurrent load" $ do
      store <- newStore
      let uid = UserId (T.pack "bob")
          cfg = RateLimitConfig
            { rlcCapacity = 3, rlcRefillRate = 0, rlcWindowSize = 10 }

      decisions <- mapConcurrently
        (const $ do
          now <- getCurrentTime
          checkAndUpdate store uid (\s -> checkFixedWindow cfg s now))
        [1 .. 50 :: Int]

      length (filter (== Allowed) decisions) `shouldBe` 3

    it "resets the count once the window has passed" $ do
      store <- newStore
      let uid = UserId (T.pack "ivy")
          cfg = RateLimitConfig
            { rlcCapacity = 2, rlcRefillRate = 0, rlcWindowSize = 5 }

      t0 <- getCurrentTime

      d1 <- checkAndUpdate store uid (\s -> checkFixedWindow cfg s t0)
      d2 <- checkAndUpdate store uid (\s -> checkFixedWindow cfg s t0)
      d3 <- checkAndUpdate store uid (\s -> checkFixedWindow cfg s t0)

      [d1, d2, d3] `shouldBe` [Allowed, Allowed, Denied]

      -- Advance time past the window -> should reset to a fresh window
      let t1 = addUTCTime 6 t0
      d4 <- checkAndUpdate store uid (\s -> checkFixedWindow cfg s t1)

      d4 `shouldBe` Allowed

  describe "Token Bucket rate limiter" $ do

    it "allows requests up to capacity, then denies, sequentially" $ do
      store <- newStore
      let uid = UserId (T.pack "carol")
          cfg = RateLimitConfig
            { rlcCapacity = 3, rlcRefillRate = 0, rlcWindowSize = 0 }

      decisions <- mapM
        (const $ do
          now <- getCurrentTime
          checkAndUpdate store uid (\s -> checkTokenBucket cfg s now))
        [1 .. 5 :: Int]

      decisions `shouldBe` [Allowed, Allowed, Allowed, Denied, Denied]

    it "never allows more than capacity, even under concurrent load" $ do
      store <- newStore
      let uid = UserId (T.pack "dave")
          cfg = RateLimitConfig
            { rlcCapacity = 3, rlcRefillRate = 0, rlcWindowSize = 0 }

      decisions <- mapConcurrently
        (const $ do
          now <- getCurrentTime
          checkAndUpdate store uid (\s -> checkTokenBucket cfg s now))
        [1 .. 50 :: Int]

      length (filter (== Allowed) decisions) `shouldBe` 3

    it "refills tokens over time, allowing requests again after waiting" $ do
      store <- newStore
      let uid = UserId (T.pack "grace")
          cfg = RateLimitConfig
            { rlcCapacity = 2, rlcRefillRate = 1.0, rlcWindowSize = 0 }
            -- capacity 2, refills 1 token per second

      t0 <- getCurrentTime

      -- Exhaust the bucket: 2 allowed, 1 denied, all "at" t0
      d1 <- checkAndUpdate store uid (\s -> checkTokenBucket cfg s t0)
      d2 <- checkAndUpdate store uid (\s -> checkTokenBucket cfg s t0)
      d3 <- checkAndUpdate store uid (\s -> checkTokenBucket cfg s t0)

      [d1, d2, d3] `shouldBe` [Allowed, Allowed, Denied]

      -- Simulate 1 second passing -> exactly 1 token should refill
      let t1 = addUTCTime 1 t0
      d4 <- checkAndUpdate store uid (\s -> checkTokenBucket cfg s t1)

      d4 `shouldBe` Allowed

      -- Immediately after, with no time passing, bucket should be empty again
      d5 <- checkAndUpdate store uid (\s -> checkTokenBucket cfg s t1)

      d5 `shouldBe` Denied

  describe "Sliding Window rate limiter" $ do

    it "allows requests up to capacity, then denies, sequentially" $ do
      store <- newStore
      let uid = UserId (T.pack "erin")
          cfg = RateLimitConfig
            { rlcCapacity = 3, rlcRefillRate = 0, rlcWindowSize = 10 }

      decisions <- mapM
        (const $ do
          now <- getCurrentTime
          checkAndUpdate store uid (\s -> checkSlidingWindow cfg s now))
        [1 .. 5 :: Int]

      decisions `shouldBe` [Allowed, Allowed, Allowed, Denied, Denied]

    it "never allows more than capacity, even under concurrent load" $ do
      store <- newStore
      let uid = UserId (T.pack "frank")
          cfg = RateLimitConfig
            { rlcCapacity = 3, rlcRefillRate = 0, rlcWindowSize = 10 }

      decisions <- mapConcurrently
        (const $ do
          now <- getCurrentTime
          checkAndUpdate store uid (\s -> checkSlidingWindow cfg s now))
        [1 .. 50 :: Int]

      length (filter (== Allowed) decisions) `shouldBe` 3
    
    it "allows requests again after old ones slide out of the window" $ do
      store <- newStore
      let uid = UserId (T.pack "henry")
          cfg = RateLimitConfig
            { rlcCapacity = 2, rlcRefillRate = 0, rlcWindowSize = 5 }
            -- capacity 2, 5 second window

      t0 <- getCurrentTime

      -- Exhaust the window at t0
      d1 <- checkAndUpdate store uid (\s -> checkSlidingWindow cfg s t0)
      d2 <- checkAndUpdate store uid (\s -> checkSlidingWindow cfg s t0)
      d3 <- checkAndUpdate store uid (\s -> checkSlidingWindow cfg s t0)

      [d1, d2, d3] `shouldBe` [Allowed, Allowed, Denied]

      -- Advance time past the 5 second window -> old requests should expire
      let t1 = addUTCTime 6 t0
      d4 <- checkAndUpdate store uid (\s -> checkSlidingWindow cfg s t1)

      d4 `shouldBe` Allowed