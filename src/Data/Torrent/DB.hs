{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Data.Torrent.DB
  ( connToDB
  , addTorrent
  , getTorrent
  )
where

import           Database.MongoDB.Query
import           Database.MongoDB
import           Data.Bson                      ( (=:) )
import           Control.Monad.Trans            ( liftIO )
import           Network.Torrent.Tracker.AnnounceReqTypes
import           Data.Torrent.DB.FromBSON
import           Data.Torrent.DB.ToBSON
import           Data.Either

import           Data.BEncode
import qualified Data.ByteString.Lazy          as BL

connToDB :: Username -> Password -> String -> IO (Maybe Pipe)
connToDB uname pw host = do
  host'  <- readHostPortM host
  pipe   <- connect host'
  isAuth <- access pipe master "torrentDB" $ auth uname pw
  if isAuth then return $ Just pipe else return Nothing

addTorrent :: BL.ByteString -> Action IO ()
addTorrent bs = do
  let doc = bencodeToBSON bs
  case doc of
    (Right gd) -> insert_ "torrents" gd
    (Left  gd) -> liftIO $ print gd

getTorrent :: [Infohash] -> Action IO ([Maybe BEncode])
getTorrent ihs = do
  let ihls  = Array $ map (\ih -> Doc ["info_hash" =: Bin (Binary ih)]) ihs
      sel   = Select { selector = ["$or" =: ihls], coll = "torrents" }
      query = Query { options   = []
                    , selection = sel
                    , project   = ["info_hash" =: 0]
                    , limit     = 0
                    , sort      = []
                    , snapshot  = False
                    , batchSize = 0
                    , hint      = []
                    , skip      = 0
                    }
  curs <- find query
  docs <- rest curs
  return $ map bsonToBencode docs
