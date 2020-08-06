{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import           Database.MongoDB
import           Database.MongoDB.Connection
import           Database.MongoDB.Admin
import           Database.MongoDB.Query
import qualified Data.Text                     as T
import           System.Environment
import           System.IO
import           Control.Exception
import           System.Process

main :: IO ()
main = do
  host <- getEnv "TorrDBHostName"
  user <- getEnv "TorrDBUserName"
  pass <- getEnv "TorrDBPassWord"
  db   <- getEnv "TorrentDB"
  coll <- getEnv "TorrColl"
  pipe <- connect $ readHostPort host
  let db'       = T.pack db
      torrents' = T.pack "torrents"
      labels    = map T.pack ["created by", "comment"]
  access pipe master db' $ auth (T.pack user) (T.pack pass)
  access pipe master db' $ createCollection [] torrents'
  let
    idx
      = "db.torrents.createIndex({'created by': 'text', 'comment': 'text', 'description' : 'text', 'tname' : 'text'})"
  readProcess "mongo" ["--host", host, db, "-u", user, "-p", pass] idx >>= print
  close pipe
  return ()
 where
  rdVal :: String -> IO String
  rdVal msg = putStr msg >> hFlush stdout >> getLine
  withEcho :: Bool -> IO a -> IO a
  withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
