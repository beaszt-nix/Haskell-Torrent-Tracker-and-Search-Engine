{-# LANGUAGE OverloadedStrings #-}

module Torrent.Conversions  where

import Control.Applicative ((<|>))
import Crypto.Hash 
import Crypto.Hash.Algorithms(SHA1(..))
import Data.ByteArray.Encoding
import qualified Data.ByteArray as BA
import qualified Data.BEncode as BE
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe)
import Torrent.Types
import Data.Time.Clock
import System.IO.Unsafe

parseSingleFile :: M.Map String BE.BEncode -> Maybe FileInfo
parseSingleFile info = do
  (BE.BString nm) <- M.lookup "name" info
  (BE.BInt l) <- M.lookup "length" info
  let md5 = case M.lookup "md5sum" info of
        Just (BE.BString bs) -> return bs
        Nothing -> Nothing
  return $
    Single $
      SingleInfo
        { singleFileLen = fromIntegral l,
          singleName = nm,
          singleMD5Sum = md5
        }

parseFile :: BE.BEncode -> Maybe File
parseFile (BE.BDict file) = do
  (BE.BInt ln) <- M.lookup "length" file
  (BE.BList pth) <- M.lookup "path" file
  let pths = map (\(BE.BString f) -> f) pth
      md5 = case M.lookup "md5sum" file of
        Just (BE.BString bs) -> return bs
        Nothing -> Nothing
  return $
    File
      { fileLength = fromIntegral ln,
        filePath = pths,
        fileMD5Sum = md5
      }
parseFile _ = Nothing

parseMultiFile :: M.Map String BE.BEncode -> Maybe FileInfo
parseMultiFile info = do
  (BE.BString nm) <- M.lookup "name" info
  (BE.BList fls) <- M.lookup "files" info
  let fls' = mapMaybe parseFile fls
  return $
    Multi $
      MultiInfo
        { dirName = nm,
          files = fls'
        }

parseInfo :: M.Map String BE.BEncode -> Maybe InfoDict
parseInfo dict = do
  (BE.BInt pcLen) <- M.lookup "piece length" dict
  (BE.BString pcs) <- M.lookup "pieces" dict
  let pvt = maybe False optfunc (M.lookup "private" dict)
  finfo <- parseSingleFile dict <|> parseMultiFile dict
  return $
    Info
      { pieceLen = fromIntegral pcLen,
        pieces = pcs,
        private = pvt,
        fileinfo = finfo
      }
  where
    optfunc :: BE.BEncode -> Bool
    optfunc (BE.BInt d) = case d of
      0 -> False
      1 -> True
    optfunc _ = False

bencodeToTorrent :: B8.ByteString -> Maybe Torrent
bencodeToTorrent bs = do
  (BE.BDict dict) <- BE.bRead bs
  (BE.BString anc) <- M.lookup "announce" dict
  infoDict@(BE.BDict infodict) <- M.lookup "info" dict
  let k = convertToBase Base64 (hashlazy $ BE.bPack infoDict :: Digest SHA1) :: B.ByteString
  ifo <- parseInfo infodict
  let createDate = take 19 $ show $ unsafePerformIO getCurrentTime
      cmt = bstring <$> M.lookup "comment" dict
      crt = maybe "Anonymous" bstring $ M.lookup "created by" dict
      ecdg = bstring <$> M.lookup "encoding" dict
      ancl = func <$> M.lookup "announce-list" dict
  return $
    Torrent
      { info = ifo,
        announce = anc,
        announceList = ancl,
        comment = cmt,
        createdBy = crt,
        encoding = ecdg,
        creationDate = B8.pack createDate,
        infoHash=B8.fromStrict k
      }
  where
    bstring (BE.BString x) = x
    bstring _ = error "Expected String"

    blist (BE.BList xs) = xs
    blist _ = error "Expected List"

    func = map (map bstring . blist) . blist

toBInt = BE.BInt . fromIntegral

packSingle :: SingleInfo -> [(String, BE.BEncode)]
packSingle s =
  let flen = toBInt $ singleFileLen s
      fname = BE.BString $ singleName s
      md5sum = fromMaybe B8.empty $ singleMD5Sum s
      list = [("md5sum", BE.BString md5sum) | B8.length md5sum /= 0]
   in [("name", fname), ("length", flen)] ++ list

packFile :: File -> BE.BEncode
packFile benF =
  let flen = toBInt $ fileLength benF
      fpath = BE.BList $ map BE.BString $ filePath benF
      md5sum = fromMaybe B8.empty $ fileMD5Sum benF
      list = [("md5sum", BE.BString md5sum) | B8.length md5sum /= 0]
      dict = list ++ [("length", flen), ("path", fpath)]
   in BE.BDict $ M.fromList dict

packMulti :: MultiInfo -> [(String, BE.BEncode)]
packMulti m =
  let mname = BE.BString $ dirName m
      mfiles = BE.BList $ map packFile $ files m
   in [("name", mname), ("files", mfiles)]

packInfo :: InfoDict -> BE.BEncode
packInfo ifo = do
  let pcLen = toBInt $ pieceLen ifo
      pcs = BE.BString $ pieces ifo
      pvt = if private ifo then BE.BInt 1 else BE.BInt 0
      ifmap = [("piece length", pcLen), ("pieces", pcs), ("private", pvt)]
      dict = case fileinfo ifo of
        Single s -> packSingle s
        Multi m -> packMulti m
   in BE.BDict $ M.fromList $ ifmap ++ dict

packOpts :: Maybe a -> (a -> b) -> String -> [(String, b)]
packOpts m f str =
  case m of
    Just k -> [(str, f k)]
    Nothing -> []

torrentToBencode :: Torrent -> B8.ByteString
torrentToBencode torr =
  let ifo = packInfo $ info torr
      anc = BE.BString $ announce torr
      cmmt = packOpts (comment torr) BE.BString "comment"
      ecdg = packOpts (encoding torr) BE.BString "encoding"
      ancL = packOpts (announceList torr) f "announce-list"
      list = concat [cmmt, ecdg, ancL, [("announce", anc), ("info", ifo)]]
   in BE.bPack $ BE.BDict $ M.fromList list
  where
    f = BE.BList . f'
    f' [] = []
    f' (x : xs) = BE.BList (map BE.BString x) : f' xs
