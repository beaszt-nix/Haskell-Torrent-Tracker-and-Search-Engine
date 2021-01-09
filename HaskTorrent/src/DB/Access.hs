{-# LANGUAGE OverloadedStrings #-}

module DB.Access where

import Control.Monad.IO.Class
import DB.Search
import Data.Bson
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Maybe
import qualified Data.Text as T
import Database.MongoDB
import Database.MongoDB.Connection
import System.Environment
import Torrent.Conversions
import Torrent.Instances
import Torrent.Types
import Types

collection :: IO T.Text
collection = T.pack <$> getEnv "TorrColl"

dbName :: IO T.Text
dbName = T.pack <$> getEnv "TorrentDB"

connectDB :: IO (Maybe Pipe)
connectDB = do
  host <- getEnv "TorrDBHostName" >>= readHostPortM
  pipe <- connect host
  dbName <- T.pack <$> getEnv "TorrentDB"
  userNm <- T.pack <$> getEnv "TorrDBUserName"
  passwd <- T.pack <$> getEnv "TorrDBPassWord"
  isAuth <- access pipe master dbName $ auth userNm passwd
  if isAuth then return $ Just pipe else close pipe >> return Nothing

uploadTorrent :: UploadReq -> Action IO B8.ByteString
uploadTorrent (UploadReq ttl desc minfo) = do
  col <- liftIO collection
  let (Doc torr) = val minfo
      wrds = getSearchIndex $ concatMap T.words [ttl, desc]
  insert_ col $
    torr
      ++ ["title" =: ttl, "description" =: desc, "ngrams" =: wrds]
  return $ fromMaybe "error" $ torr !? "infoHash"

searchTorrent :: T.Text -> Action IO [ResultCard]
searchTorrent query = do
  col <- liftIO collection
  let srchqry = getSearchIndex . T.words $ query
      sel = ["$text" =: Doc ["$search" =: T.unwords srchqry]]
      qry = select sel col :: Query
      qry' =
        qry
          { project =
              [ "_id" =: (0 :: Int),
                "title" =: (1 :: Int),
                "created by" =: (1 :: Int),
                "creation date" =: (1 :: Int),
                "infoHash" =: (1 :: Int)
              ]
          }
  curs <- find qry'
  mapMaybe (cast' . f) <$> nextN 30 curs

f doc = Doc $ doc ++ ["seed" =: (3 :: Integer), "leech" =: (1 :: Integer)]

getTorrent :: String -> Action IO (Maybe TorrentDesc)
getTorrent id' = do
  col <- liftIO collection
  let id'' = B8.pack id'
      sel = ["infoHash" =: id'']
      qry = select sel col :: Query
      qry' =
        qry
          { project =
              [ "_id" =: (0 :: Int),
                "title" =: (1 :: Int),
                "created by" =: (1 :: Int),
                "creation date" =: (1 :: Int),
                "description" =: (1 :: Int),
                "info" =: (1 :: Int),
                "infoHash" =: (1 :: Int)
              ]
          }
  res <- (f <$>) <$> findOne qry'
  return $
    if isNothing res
      then Nothing
      else do
        (Doc res') <- res
        ifo <- res' !? "info" >>= cast'
        rcard <- cast' (Doc res')
        dsc <- res' !? "description"
        return $
          TorrentDesc
            { details = rcard,
              description = dsc,
              dirTree = func $ fileinfo ifo
            }
  where
    func :: FileInfo -> [([T.Text], Integer)]
    func (Single sinfo) = [([T.pack $ B8.unpack $ singleName sinfo], fromIntegral $ singleFileLen sinfo)]
    func (Multi minfo) =
      let fname = T.pack $ B8.unpack $ dirName minfo
       in map (\f -> ((:) fname . map (T.pack . B8.unpack) . filePath $ f, fromIntegral $ fileLength f)) $ files minfo

dlTorrent :: String -> Action IO (Maybe Torrent)
dlTorrent torrID = do
  col <- liftIO collection
  let torrID' = B8.pack torrID
      sel = ["infoHash" =: torrID']
      qry = select sel col :: Query
      qry' =
        qry
          { project =
              [ "_id" =: (0 :: Int),
                "description" =: (0 :: Int),
                "title" =: (0 :: Int),
                "ngrams" =: (0 :: Int),
                "title" =: (0 :: Int)
              ]
          }
  (\x -> (Doc <$> x) >>= cast') <$> findOne qry'