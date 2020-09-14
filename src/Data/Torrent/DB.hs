{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Data.Torrent.DB
  ( connToDB
  , addTorrent
  , SearchRes(..)
  , getTorrentFromIH
  , getTorrentFromIH'
  , getTorrentFromText
  , nextNTorrent
  , nextTorrentBatch
  , restTorrent
  , nextTorrent
  , bencodeToBSON
  , torSize
  , smallSize
  , bsonToBencode
  , torrColl
  , torrDB
  )
where

import           Database.MongoDB.Query
import           Data.Fixed
import           Database.MongoDB
import           Data.Bson                      ( (=:) )
import           Control.Monad
import           Control.Monad.Trans            ( liftIO )
import           Data.Torrent.Search
import           Control.Monad.IO.Class
import           System.Environment
import           Control.Monad.Reader           ( runReaderT )
import           Network.Torrent.Tracker.AnnounceReqTypes
import qualified Data.ByteString.Base16        as B16
import           Data.Torrent.DB.FromBSON
import           Data.Torrent.DB.ToBSON
import           Data.Either
import           Data.Maybe
import           Data.BEncode
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Char8         as B
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

smallSize :: Integer -> T.Text
smallSize x =
  let kb = (fromIntegral x / 1024) :: Fixed E3
      mb = (kb / 1024)
      gb = (mb / 1024)
  in  if kb > 1024
        then if mb > 1024
          then T.pack $ (show gb ++ "GB")
          else T.pack $ (show mb ++ "MB")
        else T.pack $ (show kb ++ "KB")

torSize :: Document -> Value
torSize doc =
  let (Doc   info ) = fromMaybe (Doc []) $ look "info" doc
      (Array files) = fromMaybe (Array []) $ look "files" info
      files' = map (\(Doc a) -> fromMaybe (Int64 0) $ look "length" a) files
      output        = foldr (\(Int64 x) b -> (fromIntegral x) + b) 0 files'
  in  (String $ smallSize output)

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

addTorrent :: T.Text -> BL.ByteString -> SearchEnv -> Action IO String
addTorrent tname bs se = do
  let doc = bencodeToBSON bs
      cus = ["tname" =: tname]
  col <- liftIO $ torrColl
  case doc of
    (Right gd) -> do
      let res = f . valueAt (T.pack "info_hash") $ gd
          qur = cus ++ gd
      ngrams <- liftIO $ runReaderT (genDocNgram qur) se
      insert_ col (qur ++ ["ngrams" =: ngrams, "torrent_size" =: torSize gd])
      return res
    (Left gd) -> return "Failed."
 where
  f (Bin (Binary x)) = B.unpack . B16.encode $ x
  f _                = "Failed."

getTorrentFromText :: T.Text -> SearchEnv -> Action IO Cursor
getTorrentFromText text se = do
  col <- liftIO torrColl
  src <- liftIO $ runReaderT (mkQuery text) se

  let sel    = ["$text" =: Doc ["$search" =: src]]
      query  = select sel col :: Query
      query' = query
        { project = [ "info_hash" =: 1
                    , "tname" =: 1
                    , "created by" =: 1
                    , "comment" =: 1
                    , "torrent_size" =: 1
                    , "_id" =: 0
                    ]
        }
  curs <- find query'
  return curs

nextNTorrent :: MonadIO m => Int -> Cursor -> Action m [SearchRes]
nextNTorrent i curs = nextN i curs >>= return . catMaybes . map docToSearchRes

nextTorrent :: MonadIO m => Cursor -> Action m (Maybe SearchRes)
nextTorrent c = next c >>= return . docToSearchRes . fromMaybe []

nextTorrentBatch :: MonadIO m => Cursor -> Action m [SearchRes]
nextTorrentBatch c = nextBatch c >>= return . catMaybes . map docToSearchRes

restTorrent :: MonadIO m => Cursor -> Action m [SearchRes]
restTorrent c = rest c >>= return . catMaybes . map docToSearchRes

getTorrentFromIH' :: Infohash -> Action IO Document
getTorrentFromIH' ihs = do
  col <- liftIO torrColl
  let ihls   = ["info_hash" =: Bin (Binary (w160toBString ihs))]
      query  = select ihls col :: Query
      query' = query { project = ["_id" =: 0] }
  curs <- findOne query'
  return $ fromMaybe [] curs

getTorrentFromIH :: Infohash -> Action IO (Maybe BEncode)
getTorrentFromIH ihs = do
  res <- getTorrentFromIH' ihs
  return $ bsonToBencode res
