{-# LANGUAGE OverloadedStrings #-}

module Torrent.Instances where

import           Data.Aeson                     ( (.=) )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson
import           Data.Bson                      ( Binary(Binary)
                                                , Field(label, value)
                                                , MD5(MD5)
                                                , Val(cast', val)
                                                , Value
                                                  ( Array
                                                  , Bin
                                                  , Bool
                                                  , Doc
                                                  , Float
                                                  , Int32
                                                  , Int64
                                                  , Md5
                                                  , Null
                                                  , String
                                                  , UTC
                                                  )
                                                , look
                                                , (!?)
                                                , (=:)
                                                )
import qualified Data.ByteString.Lazy.Char8    as B8
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( )
import qualified Data.Text                     as T
import           Data.Word                      ( Word64 )
import           Torrent.Types                  ( File(..)
                                                , FileInfo(..)
                                                , InfoDict(..)
                                                , MultiInfo(..)
                                                , ResultCard(..)
                                                , SingleInfo(..)
                                                , Torrent(..)
                                                , TorrentDesc(..)
                                                )

valMd5 (Just bs) = ["md5sum" =: (Md5 . MD5 . B8.toStrict $ bs)]
valMd5 Nothing   = []

castMd5 :: Maybe Value -> Maybe B8.ByteString
castMd5 (Just (Md5 (MD5 val))) = Just (B8.fromStrict val)
castMd5 _                      = Nothing

instance Val Word64 where
  val wd = val (fromIntegral wd :: Integer)
  cast' wd = fromInteger <$> cast' wd

instance Val B8.ByteString where
  val bs = val (B8.unpack bs)
  cast' bs = B8.pack <$> cast' bs

instance Val SingleInfo where
  val sinfo =
    let sname = val $ singleName sinfo
        slen  = val $ singleFileLen sinfo
        smd5  = valMd5 $ singleMD5Sum sinfo
    in  Doc $ ["name" =: sname, "length" =: slen] ++ smd5
  cast' (Doc doc) = do
    nom  <- B8.pack <$> (doc !? "name")
    slen <- fromInteger <$> (doc !? "length")
    let res = castMd5 $ look "md5sum" doc
    return $ SingleInfo { singleName    = nom
                        , singleFileLen = slen
                        , singleMD5Sum  = res
                        }
  cast' _ = Nothing

instance Val File where
  val file =
    let flen  = val $ fileLength file
        fpath = val $ filePath file
        fmd5  = valMd5 $ fileMD5Sum file
    in  Doc $ ["length" =: flen, "path" =: fpath] ++ fmd5
  cast' (Doc doc) = do
    flen  <- doc !? "length"
    fpath <- doc !? "path"
    let res = castMd5 $ look "md5sum" doc
    return $ File { fileLength = flen, filePath = fpath, fileMD5Sum = res }
  cast' _ = Nothing

instance Val MultiInfo where
  val minfo =
    let dname = val $ dirName minfo
        fls   = val $ files minfo
    in  Doc ["name" =: dname, "files" =: fls]
  cast' (Doc doc) = do
    dname <- doc !? "name"
    fls   <- doc !? "files"
    return $ MultiInfo { dirName = dname, files = fls }
  cast' _ = Nothing

instance Val FileInfo where
  val (Multi  m) = let (Doc d) = val m in Doc $ d ++ ["multi" =: True]
  val (Single s) = let (Doc d) = val s in Doc $ d ++ ["multi" =: False]
  cast' d@(Doc doc) = do
    test <- doc !? "multi"
    if test then Multi <$> cast' d else Single <$> cast' d
  cast' _ = Nothing

instance Val InfoDict where
  val ifd =
    let pcLen = val $ pieceLen ifd
        pcs   = val $ pieces ifd
        pvt   = val $ private ifd
        finfo = val $ fileinfo ifd
    in  Doc
          [ "piece length" =: pcLen
          , "pieces" =: pcs
          , "private" =: pvt
          , "fileInfo" =: finfo
          ]
  cast' (Doc doc) = do
    pcLen <- doc !? "piece length"
    pcs   <- doc !? "pieces"
    pvt   <- doc !? "private"
    finfo <- doc !? "fileInfo"
    return $ Info pcLen pcs pvt finfo
  cast' _ = Nothing

instance Val Torrent where
  val torr =
    let ifo  = val $ info torr
        anc  = val $ announce torr
        ifH  = val $ infoHash torr
        date = val $ creationDate torr
        ancL = f "announce-list" $ announceList torr
        ownr = createdBy torr
        cmmt = f "comment" $ comment torr
        ecdg = f "encoding" $ encoding torr
    in  Doc $ concat
          [ ancL
          , cmmt
          , [ "created by" =: ownr
            , "creation date" =: date
            , "info" =: ifo
            , "announce" =: anc
            , "infoHash" =: ifH
            ]
          ]
    where f s = maybe [] (\x -> [s =: val x])
  cast' (Doc doc) = do
    ifo  <- doc !? "info"
    anc  <- doc !? "announce"
    ifH  <- doc !? "infoHash"
    date <- doc !? "creation date"
    ownr <- doc !? "created by"
    let ancL = doc !? "announce-list"
        cmmt = doc !? "comment"
        ecdg = doc !? "encoding"
    return $ Torrent { info         = ifo
                     , announce     = anc
                     , announceList = ancL
                     , creationDate = date
                     , createdBy    = ownr
                     , comment      = cmmt
                     , encoding     = ecdg
                     , infoHash     = ifH
                     }
  cast' _ = Nothing

instance Val ResultCard where
  val resultCard =
    let ttl  = val $ title resultCard
        sd   = val $ seed resultCard
        lc   = val $ leech resultCard
        cr   = val $ creator resultCard
        upDt = val $ uploadDate resultCard
        id'  = val $ show $ torrID resultCard
    in  Doc
          [ "title" =: ttl
          , "seed" =: sd
          , "leech" =: lc
          , "created by" =: cr
          , "creation date" =: upDt
          , "infoHash" =: id'
          ]
  cast' (Doc d) = do
    ttl  <- d !? "title"
    sd   <- d !? "seed"
    lc   <- d !? "leech"
    cr   <- d !? "created by"
    upDt <- d !? "creation date"
    id'  <- d !? "infoHash"
    return $ ResultCard ttl sd lc cr upDt id'
  cast' _ = Nothing

instance (Val a, Val b) => Val (a,b) where
  val (a,b)= Doc ["fst" =: a, "snd" =: b]
  cast' (Doc l) = do
    a <- l !? "fst"
    b <- l !? "snd"
    return (a,b)

instance Val TorrentDesc where
  val desc = Doc
    [ "details" =: (val . details) desc
    , "description" =: (val . description) desc
    , "dirtree" =: (val . dirTree) desc
    ]
  cast' (Doc d) = do
    dts  <- d !? "details"
    desc <- d !? "description"
    dirT <- d !? "dirtree"
    return $ TorrentDesc dts desc dirT
  cast' _ = Nothing

convField :: Field -> (T.Text, Aeson.Value)
convField field =
  let l = label field
      v = value field
  in  (l .= Aeson.toJSON v)

instance Aeson.ToJSON Value where
  toJSON (Float  f          ) = Aeson.toJSON f
  toJSON (String s          ) = Aeson.toJSON s
  toJSON (Bin    (Binary bs)) = Aeson.toJSON $ B8.unpack $ B8.fromStrict bs
  toJSON (Bool   b          ) = Aeson.toJSON b
  toJSON (UTC    t          ) = Aeson.toJSON t
  toJSON Null                 = Aeson.Null
  toJSON (Int32 i32)          = Aeson.toJSON i32
  toJSON (Int64 i64)          = Aeson.toJSON i64
  toJSON (Doc   d  )          = (Aeson.toJSON . M.fromList . map convField) d
  toJSON (Array a  )          = Aeson.toJSON a
  toJSON _ = Aeson.object ["error" .= ("Undefined" :: String)]

mkValJSON :: Val a => a -> Aeson.Value
mkValJSON v = Aeson.toJSON $ val v

instance Aeson.ToJSON ResultCard where
  toJSON rcard = Aeson.toJSON $ val rcard

instance Aeson.ToJSON TorrentDesc where
  toJSON desc = Aeson.toJSON $ val desc
