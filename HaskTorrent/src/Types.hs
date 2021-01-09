{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Data.Aeson.Types
import           Torrent.Conversions
import           Torrent.Types
import           Control.Monad.IO.Class

import qualified Data.ByteString.Lazy.Char8    as B8
import qualified Data.Text                     as T

data UploadReq = UploadReq
  { title :: T.Text,
    desc :: T.Text,
    metainfo :: Torrent
  }
  deriving (Eq, Show)

func :: IO String
func = return "jeff"

instance FromJSON UploadReq where
  parseJSON (Object m) = do
    ttl        <- m .: "title"
    desc       <- m .: "description"
    (Just tbs) <- bencodeToTorrent . B8.pack <$> m .: "metainfo"
    return $ UploadReq { Types.title = ttl, desc = desc, metainfo = tbs }
  parseJSON invalid =
    prependFailure "parsing Coord failed, " (typeMismatch "Object" invalid)
