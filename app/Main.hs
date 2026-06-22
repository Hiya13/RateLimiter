module Main where

import RateLimiter.Types (UserId (..), Decision (..))
import qualified Data.Text as T

main :: IO ()
main = do
  let uid = UserId (T.pack "user123")
  putStrLn ("Created user id: " ++ show uid)
  print Allowed