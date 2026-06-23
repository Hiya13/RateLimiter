module Main where

import Network.Wai.Handler.Warp (run)
import Servant

import RateLimiter.Types
import RateLimiter.AppContext
import RateLimiter.Api (API, server)

main :: IO ()
main = do
  let defaultConfig = RateLimitConfig
        { rlcCapacity   = 5
        , rlcRefillRate = 1.0   -- 1 token per second, for token bucket
        , rlcWindowSize = 10    -- 10 second window, for fixed/sliding window
        }

  ctx <- newAppContext defaultConfig

  putStrLn "Rate limiter API running on http://localhost:8080"
  run 8080 (serve (Proxy :: Proxy API) (server ctx))