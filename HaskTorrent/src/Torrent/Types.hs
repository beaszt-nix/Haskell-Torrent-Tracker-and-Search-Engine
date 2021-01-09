{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Torrent.Types where

import qualified Data.ByteString.Lazy.Char8    as B8
import qualified Data.Text                     as T
import qualified Data.Set                      as S
import           Data.Word                      ( Word64 )
import           Data.Bson

data FileInfo
  = Single SingleInfo
  | Multi MultiInfo
  deriving (Eq, Show)

data SingleInfo = SingleInfo
  { singleName :: B8.ByteString,
    singleFileLen :: Word64,
    singleMD5Sum :: Maybe B8.ByteString
  }
  deriving (Eq, Show)

data File = File
  { fileLength :: Word64,
    fileMD5Sum :: Maybe B8.ByteString,
    filePath :: [B8.ByteString]
  }
  deriving (Eq, Show)

data MultiInfo = MultiInfo
  { dirName :: B8.ByteString,
    files :: [File]
  }
  deriving (Eq, Show)

data InfoDict = Info
  { pieceLen :: Word64,
    pieces :: B8.ByteString,
    private :: Bool,
    fileinfo :: FileInfo
  }
  deriving (Eq, Show)

data Torrent = Torrent
  { info :: InfoDict,
    infoHash :: B8.ByteString,
    announce :: B8.ByteString,
    announceList :: Maybe [[B8.ByteString]],
    creationDate :: B8.ByteString,
    comment :: Maybe B8.ByteString,
    createdBy :: B8.ByteString,
    encoding :: Maybe B8.ByteString
  }
  deriving (Eq, Show)

data ResultCard = ResultCard
  { title :: String,
    seed :: Integer,
    leech :: Integer,
    creator :: String,
    uploadDate :: String,
    torrID :: B8.ByteString
  }
  deriving (Eq, Show)

data TorrentDesc = TorrentDesc
  { details :: ResultCard,
    description :: String,
    dirTree :: [([T.Text], Integer)]
  }
  deriving (Eq, Show)
