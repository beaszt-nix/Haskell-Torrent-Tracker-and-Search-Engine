{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ExtendedDefaultRules  #-}

module Data.Torrent.DB.ToBSON
  ( bencodeToBSON
  )
where

import           Data.BEncode.Reader
import           Data.BEncode
import           Data.List
import           Data.Maybe

import           Data.Either                    ( either )
import           Data.Bson                      ( (=:) )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Time.Clock.Compat         ( secondsToNominalDiffTime )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Crypto.Hash.SHA1               ( hashlazy )

import qualified Data.Map.Strict               as M
import qualified Data.Bson                     as Bson
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as B

-- Takes Bencoded Lazy Bytestring and produces BSON doc ready to be uploaded.
-- Adds Info_Hash to the metadata (For Working with LIVE Torrents)
bencodeToBSON :: BL.ByteString -> Bson.Document
bencodeToBSON bs =
  let bDict@(BDict bmap) = fromMaybe (BString "") . bRead $ bs
      infoHash =
          hashlazy . bPack . fromMaybe (BString "Failed") . M.lookup "info" $ bmap
  in  either fail (succ infoHash) . runBReader btorrent $ bDict
 where
  bsonBin x = Bson.Bin (Bson.Binary x)
  succ :: B.ByteString -> Bson.Document -> Bson.Document
  succ infohash val = concat [["info_hash" =: bsonBin infohash], val]
  fail :: String -> Bson.Document
  fail _ = ["Parse Error" =: sToText "Malformed Torrent File"]

toBinary :: BL.ByteString -> Bson.Value
toBinary x = Bson.Bin (Bson.Binary $ toStrict x)

-- Primary Torrent Parser 
btorrent :: BReader Bson.Document
btorrent = do
  info      <- dict "info" binfo
  announce  <- dict "announce" bstring
  creation  <- optional $ dict "creation date" bint
  comment   <- optional $ dict "comment" bstring
  createdBy <- optional $ dict "created by" bstring
  encoding  <- optional $ dict "encoding" bstring
  let announce'  = ["announce" =: sToText announce]
      info'      = ["info" =: info]
      creation'  = fromOptional creation "creation date" timeConv
      comment'   = fromOptional comment "comment" sToText
      createdBy' = fromOptional createdBy "created by" sToText
      encoding'  = fromOptional encoding "encoding" sToText
  return $ concat [announce', creation', comment', createdBy', encoding', info']
 where
  timeConv =
    Bson.UTC . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral

-- Helper Functions to Convert into BSON compatible Constructors
sToText = Bson.String . T.pack
sToInt = Bson.Int32 . fromInteger

-- Parser for individual file dictionary
bfile :: BReader Bson.Value
bfile = do
  len    <- dict "length" bint
  path   <- dict "path" (list bstring)
  md5sum <- optional $ dict "md5sum" bstring
  let md5 = fromMaybe "" md5sum
      li  = case length md5 of
        32 -> ["md5sum" =: sToText md5]
        _  -> []
      pth = intercalate "/" path
  return $ Bson.Doc $ ["length" =: sToInt len, "path" =: sToText pth] ++ li

-- Parses optional arguments that may or may not be present in MetaInfo file
fromOptional :: Maybe a -> String -> (a -> Bson.Value) -> Bson.Document
fromOptional (Just a) str f = [(T.pack str) =: f a]
fromOptional Nothing  _   _ = []

-- Parses the Info dictionary inside the MetaInfo
binfo :: BReader Bson.Value
binfo = do
  pieceLength <- dict "piece length" bint
  pieces      <- dict "pieces" bbytestring
  private     <- optional $ dict "private" bint
  filesM      <- optional $ bmultifile
  filesS      <- optional $ bsinglefile
  let files = case (filesM, filesS) of
        (Nothing          , Nothing          ) -> undefined
        (Just (Bson.Doc x), Nothing          ) -> x
        (Nothing          , Just (Bson.Doc x)) -> x
      pvt = fromOptional private "private" sToInt
  return $ Bson.Doc $ concat
    [ ["piece length" =: sToInt pieceLength, "pieces" =: toBinary pieces]
    , files
    , pvt
    ]

-- Parses the list of file dictionaries 
bmultifile :: BReader Bson.Value
bmultifile = do
  name  <- dict "name" bstring
  files <- dict "files" (list bfile)
  return $ Bson.Doc $ ["name" =: sToText name, "files" =: Bson.Array files]

-- Parses the MetaInfo when it is in Single File mode
bsinglefile :: BReader Bson.Value
bsinglefile = do
  name   <- dict "name" bstring
  len    <- dict "length" bint
  md5sum <- optional $ dict "md5sum" bstring
  let md5 = fromMaybe "" md5sum
      li  = case length md5 of
        32 -> ["md5sum" =: sToText md5]
        _  -> []
  return $ Bson.Doc $ ["name" =: sToText name, "length" =: sToInt len] ++ li
