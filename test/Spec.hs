module Main where

import Test.Hspec
import Control.Concurrent.Async (mapConcurrently)
import Data.Time (getCurrentTime)
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