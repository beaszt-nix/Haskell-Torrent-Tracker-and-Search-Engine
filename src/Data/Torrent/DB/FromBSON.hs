{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Data.Torrent.DB.FromBSON
  ( bsonToBencode
  , docToSearchRes
  , SearchRes(..)
  )
where

import           Control.Monad
import           Network.Torrent.Tracker.AnnounceReqTypes
                                                ( Infohash(..) )
import           Data.BEncode
import           Data.List
import           Data.List.Split                ( splitOn )
import           Data.Maybe
import           Data.Either                    ( either )
import           Data.Torrent.DB.BsonValueReader
import           Data.Binary
import           Data.ByteString.Lazy           ( fromStrict )
import qualified Data.Text                     as T
import qualified Data.Bson                     as Bson
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.ByteString.Char8         as B
import qualified Data.Map.Strict               as M

data SearchRes = SearchRes {
                              srInfoHash :: Infohash,
                              srTorrName :: T.Text,
                              srComment  :: Maybe T.Text,
                              srCreatedBy :: Maybe T.Text,
                              srSize :: T.Text
                           } deriving Show

optText :: Bson.Value -> Maybe T.Text
optText (Bson.String t) = return t
optText _               = Nothing

docToSearchRes :: Bson.Document -> Maybe SearchRes
docToSearchRes doc = do
  (Bson.Bin    (Bson.Binary x)) <- Bson.look "info_hash" doc
  (Bson.String torrName       ) <- Bson.look "tname" doc
  (Bson.String size           ) <- Bson.look "torrent_size" doc

  let comment = Bson.look "comment" doc >>= optText
      creator = Bson.look "created by" doc >>= optText

  return $ SearchRes { srInfoHash  = decode $ fromStrict x
                     , srTorrName  = torrName
                     , srComment   = comment
                     , srCreatedBy = creator
                     , srSize      = size
                     }

toBString :: String -> Maybe BEncode
toBString = Just . BString . BL.pack

toBInt :: Integer -> Maybe BEncode
toBInt x = Just . BInt $ x

md5proc :: Bson.Value -> Maybe BEncode
md5proc x = runBsonValueReader bsonstring x >>= toBString

bsonFile :: Bson.Value -> Maybe BEncode
bsonFile (Bson.Doc file) = do
  len    <- Bson.look "length" file >>= runBsonValueReader bsonint >>= toBInt
  path   <- Bson.look "path" file >>= runBsonValueReader bsonstring >>= listProc
  md5sum <- fromOptional "md5sum" md5proc $ Bson.look "md5sum" file

  let bDict = M.fromList . concat $ [[("length", len), ("path", path)], md5sum]

  return $ BDict $ bDict
  where listProc x = BList <$> (sequence . map toBString . splitOn "/") x
bsonFile _ = Nothing


bsoninfo :: Bson.Value -> Maybe BEncode
bsoninfo (Bson.Doc info) = do
  pieceLength <-
    Bson.look "piece length" info >>= runBsonValueReader bsonint >>= toBInt
  pieces <-
    BString <$> (Bson.look "pieces" info >>= runBsonValueReader bsonbinary)
  name <- Bson.look "name" info >>= runBsonValueReader bsonstring >>= toBString
  private <- fromOptional "private" privF $ Bson.look "private" info
  files <- fromOptional "files" fileMultiple $ Bson.look "files" info

  let infoDict =
        [("piece length", pieceLength), ("pieces", pieces), ("name", name)]
          ++ private
          ++ files

  case files of
    [] -> do
      len <- Bson.look "length" info >>= runBsonValueReader bsonint >>= toBInt
      md5sum <- fromOptional "md5sum" md5proc $ Bson.look "md5sum" info
      return
        $ BDict
        . M.fromList
        . concat
        $ [infoDict, md5sum, [("length", len)]]

    x -> return $ BDict . M.fromList $ infoDict

 where
  privF x = runBsonValueReader bsonint x >>= toBInt
  fileMultiple :: Bson.Value -> Maybe BEncode
  fileMultiple (Bson.Array x) = BList <$> (sequence . map bsonFile) x
  fileMultiple _              = Nothing
bsoninfo _ = Nothing

bsonToBencode :: Bson.Document -> Maybe BEncode
bsonToBencode doc = do
  info     <- Bson.look "info" doc >>= bsoninfo
  announce <-
    Bson.look "announce" doc >>= runBsonValueReader bsonstring >>= toBString
  creationBy   <- fromOptional "created by" creatF $ Bson.look "created by" doc
  creationDate <- fromOptional "creation date" dateF
    $ Bson.look "creation date" doc
  comment  <- fromOptional "comment" commenF $ Bson.look "comment" doc
  encoding <- fromOptional "encoding" encF $ Bson.look "encoding" doc
  return
    $ BDict
    . M.fromList
    . concat
    $ [ [("info", info), ("announce", announce)]
      , creationDate
      , comment
      , encoding
      ]
 where
  creatF x = runBsonValueReader bsonstring x >>= toBString
  dateF x = runBsonValueReader bsontime x >>= toBInt
  commenF x = runBsonValueReader bsonstring x >>= toBString
  encF x = runBsonValueReader bsonstring x >>= toBString
