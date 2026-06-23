{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module RateLimiter.Api
  ( API
  , server
  ) where

import Servant
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import qualified Data.Text as T

import RateLimiter.Types
import RateLimiter.Store
import RateLimiter.AppContext
import RateLimiter.Algorithm.FixedWindow (checkFixedWindow)
import RateLimiter.Algorithm.TokenBucket (checkTokenBucket)
import RateLimiter.Algorithm.SlidingWindow (checkSlidingWindow)

-- | The full API surface:
--   POST /limit/:algorithm/:userId   -> check rate limit, returns Decision
--   GET  /config/:userId             -> fetch user's config
--   PUT  /config/:userId             -> set user's config
type API =
       "limit" :> Capture "algorithm" String :> Capture "userId" String :> Post '[JSON] Decision
  :<|> "config" :> Capture "userId" String :> Get '[JSON] RateLimitConfig
  :<|> "config" :> Capture "userId" String :> ReqBody '[JSON] RateLimitConfig :> Put '[JSON] NoContent

-- | The Servant server, parameterized over our AppContext.
server :: AppContext -> Server API
server ctx =
       checkLimitHandler
  :<|> getConfigHandler
  :<|> setConfigHandler
  where
    checkLimitHandler :: String -> String -> Handler Decision
    checkLimitHandler algoStr userStr = do
      let uid = UserId (T.pack userStr)
      cfg <- liftIO (getUserConfig ctx uid)
      now <- liftIO getCurrentTime
      case algoStr of
        "fixed-window" ->
          liftIO $ checkAndUpdate (ctxFixedWindowStore ctx) uid
            (\s -> checkFixedWindow cfg s now)
        "token-bucket" ->
          liftIO $ checkAndUpdate (ctxTokenBucketStore ctx) uid
            (\s -> checkTokenBucket cfg s now)
        "sliding-window" ->
          liftIO $ checkAndUpdate (ctxSlidingWindowStore ctx) uid
            (\s -> checkSlidingWindow cfg s now)
        _ -> throwError err400 { errBody = "Unknown algorithm. Use fixed-window, token-bucket, or sliding-window." }

    getConfigHandler :: String -> Handler RateLimitConfig
    getConfigHandler userStr = do
      let uid = UserId (T.pack userStr)
      liftIO (getUserConfig ctx uid)

    setConfigHandler :: String -> RateLimitConfig -> Handler NoContent
    setConfigHandler userStr cfg = do
      let uid = UserId (T.pack userStr)
      liftIO (setUserConfig ctx uid cfg)
      pure NoContent