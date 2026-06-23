module RateLimiter.AppContext
  ( AppContext (..)
  , newAppContext
  , Algorithm (..)
  , getUserConfig
  , setUserConfig
  ) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map

import RateLimiter.Types
import RateLimiter.Store

-- | Which algorithm to use for a given rate-limit check.
data Algorithm = FixedWindow | TokenBucket | SlidingWindow
  deriving (Eq, Show)

-- | Bundles everything the API handlers need: one Store per algorithm
-- (since each has a different state type), plus per-user configs.
data AppContext = AppContext
  { ctxFixedWindowStore   :: Store FixedWindowState
  , ctxTokenBucketStore   :: Store TokenBucketState
  , ctxSlidingWindowStore :: Store SlidingWindowState
  , ctxConfigs            :: TVar (Map.Map UserId RateLimitConfig)
  , ctxDefaultConfig      :: RateLimitConfig
  }

-- | Create a fresh AppContext with empty stores and a given default config
-- (used for any user who hasn't explicitly set their own config yet).
newAppContext :: RateLimitConfig -> IO AppContext
newAppContext defaultCfg = do
  fwStore  <- newStore
  tbStore  <- newStore
  swStore  <- newStore
  configs  <- newTVarIO Map.empty
  pure AppContext
    { ctxFixedWindowStore   = fwStore
    , ctxTokenBucketStore   = tbStore
    , ctxSlidingWindowStore = swStore
    , ctxConfigs            = configs
    , ctxDefaultConfig      = defaultCfg
    }

-- | Look up a user's config, falling back to the default if unset.
getUserConfig :: AppContext -> UserId -> IO RateLimitConfig
getUserConfig ctx uid = do
  m <- readTVarIO (ctxConfigs ctx)
  pure (Map.findWithDefault (ctxDefaultConfig ctx) uid m)

-- | Set (or overwrite) a user's config.
setUserConfig :: AppContext -> UserId -> RateLimitConfig -> IO ()
setUserConfig ctx uid cfg =
  atomically $ modifyTVar' (ctxConfigs ctx) (Map.insert uid cfg)