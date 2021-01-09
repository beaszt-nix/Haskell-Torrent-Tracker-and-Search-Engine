module Main where

import           Data.Maybe
import           Servant
import           Network.Wai.Handler.Warp
import           Lib
import           DB.Access

main :: IO ()
main = do
  pipe <- connectDB
  if isNothing pipe
    then return ()
    else do
      startApp $ fromJust pipe
