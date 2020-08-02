{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Data.Torrent.DB
  ( connToDB
  , addTorrent
  , getTorrentFromIH
  , getTorrentFromText
  , nextNTorrent
  , nextTorrentBatch
  , restTorrent
  , nextTorrent
  , bencodeToBSON
  , bsonToBencode
  , torrColl
  , torrDB
  )
where

import           Database.MongoDB.Query
import           Database.MongoDB
import           Data.Bson                      ( (=:) )
import           Control.Monad
import           Control.Monad.Trans            ( liftIO )
import           Control.Monad.IO.Class
import           System.Environment
import           Network.Torrent.Tracker.AnnounceReqTypes
import           Data.Torrent.DB.FromBSON
import           Data.Torrent.DB.ToBSON
import           Data.Either
import           Data.Maybe
import           Data.BEncode
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T

torrColl :: IO T.Text
torrColl = liftM T.pack $ getEnv "TorrColl"

torrDB :: IO T.Text
torrDB = liftM T.pack $ getEnv "TorrentDB"

dbHost :: IO String
dbHost = getEnv "TorrDBHostName"

dbUname :: IO T.Text
dbUname = liftM T.pack $ getEnv "TorrDBUserName"

dbPassW :: IO T.Text
dbPassW = liftM T.pack $ getEnv "TorrDBPassWord"

connToDB :: IO (Maybe Pipe)
connToDB = do
  host   <- dbHost
  host'  <- readHostPortM host
  pipe   <- connect host'
  db     <- torrDB
  uname  <- dbUname
  pw     <- dbPassW
  isAuth <- access pipe master db $ auth uname pw
  if isAuth then return $ Just pipe else close pipe >> return Nothing

addTorrent :: BL.ByteString -> Action IO Bool
addTorrent bs = do
  let doc = bencodeToBSON bs
  col <- liftIO $ torrColl
  case doc of
    (Right gd) -> insert_ col gd >> return True
    (Left  gd) -> liftIO $ print gd >> return False

nextNTorrent :: MonadIO m => Int -> Cursor -> Action m [BEncode]
nextNTorrent i curs = nextN i curs >>= return . catMaybes . map bsonToBencode

nextTorrent :: MonadIO m => Cursor -> Action m (Maybe BEncode)
nextTorrent c = next c >>= return . bsonToBencode . fromMaybe []

nextTorrentBatch :: MonadIO m => Cursor -> Action m [BEncode]
nextTorrentBatch c = nextBatch c >>= return . catMaybes . map bsonToBencode

restTorrent :: MonadIO m => Cursor -> Action m [BEncode]
restTorrent c = rest c >>= return . catMaybes . map bsonToBencode

getTorrentFromText :: [T.Text] -> Action IO Cursor
getTorrentFromText text = do
  col <- liftIO torrColl
  let sel = Array
        $ map (\txt -> Doc $ ["$search" =: Database.MongoDB.String txt]) text
      query  = select ["$or" =: sel] col :: Query
      query' = query { project = ["info_hash" =: 0] }
  curs <- find query'
  return curs

getTorrentFromIH :: [Infohash] -> Action IO Cursor
getTorrentFromIH ihs = do
  col <- liftIO torrColl
  let ihls = Array $ map
        (\ih -> Doc ["info_hash" =: Bin (Binary (w160toBString ih))])
        ihs
      query  = select ["$or" =: ihls] col :: Query
      query' = query { project = ["info_hash" =: 0] }
  curs <- find query'
  return curs
