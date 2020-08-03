{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Torrent.Tracker.AnnounceReqTypes
  ( AnnounceReq(..)
  , AnnounceRes(..)
  , Event(..)
  , Infohash
  , Peer(..)
  , PeerID
  , ScrapeReq(..)
  , ScrapeRes(..)
  , bencodeRes
  , bencodeScrapes
  , bencodeScrape
  , bencodePeers
  , emptyScrapeRes
  , isRFC1918
  , w160toBString
  )
where

import           Data.ByteString.Lazy           ( toStrict
                                                , fromStrict
                                                )
import           Control.Monad
import qualified Data.ByteString.Char8         as B8
import           Data.Binary
import           Data.Binary.Builder
import           Data.Binary.Put                ( runPut )
import qualified Data.ByteString               as B
import qualified Data.BEncode                  as BE
import qualified Data.Map.Strict               as M
import qualified Data.HashMap.Strict           as HM
import           Data.Word
import           Network.Socket
import           Data.Digest.SHA1
import           Data.Bits
import           Data.Hashable

instance Hashable Word160 where
  hashWithSalt salt w160 = hashWithSalt salt $ encode w160

instance Ord Word160 where
  compare (Word160 a1 b1 c1 d1 e1) (Word160 a2 b2 c2 d2 e2) =
    case compare a1 a2 of
      EQ -> case compare b1 b2 of
        EQ -> case compare c1 c2 of
          EQ -> case compare d1 d2 of
            EQ -> compare e1 e2
            x  -> x
          x -> x
        x -> x
      x -> x

instance Binary Word160 where
  get = liftM5 Word160 get get get get get
  put (Word160 a b c d e) = do
    put a
    put b
    put c
    put d
    put e

type Infohash = Word160
type PeerID = Word160

w160toBString :: Word160 -> B.ByteString
w160toBString = toStrict . runPut . put

data Event = Started | Completed | Stopped
    deriving (Eq, Ord, Show)

data Peer = Peer {
    peerID :: ! PeerID,
    peerAddr :: ! SockAddr
} deriving (Eq, Ord, Show)

data AnnounceReq = AnnounceReq {
        anInfoHash :: !Infohash,
        anPeer :: !Peer,
        anUploaded :: !Word64,
        anDownloaded :: !Word64,
        anLeft :: ! Word64,
        anEvent :: ! (Maybe Event),
        anWant :: ! (Maybe Word32)
} deriving (Eq, Ord, Show)

data AnnounceRes =
       Failure ! B.ByteString
     | PeerList {
        plInterval :: ! Word32,
        plSeeders :: ! (Maybe Word32),
        plLeechers :: ! (Maybe Word32),
        plPeers :: [Peer]
} deriving (Eq, Ord, Show)

type ScrapeReq = Infohash
data ScrapeRes = ScrapeRes {
        srSeeders :: ! Word32,
        srCompletions :: !Word32,
        srLeechers :: !Word32
} deriving (Eq, Ord, Show)

w32toInteger :: Word32 -> BE.BEncode
w32toInteger x = (BE.BInt . read . show) x

bencodeRes :: AnnounceRes -> B.ByteString
bencodeRes (Failure msg) = (toStrict . BE.bPack . BE.BDict)
  $ M.fromList [("failure", (BE.BString $ fromStrict msg))]
bencodeRes (PeerList itvl (Just seed) (Just leech) peers) =
  (toStrict . BE.bPack . BE.BDict) $ M.fromList
    [ ("interval"  , (w32toInteger itvl))
    , ("peers"     , (bencodePeers peers))
    , ("complete"  , (w32toInteger seed))
    , ("incomplete", (w32toInteger leech))
    ]
bencodeRes (PeerList itvl Nothing Nothing peers) =
  (toStrict . BE.bPack . BE.BDict) $ M.fromList
    [("interval", (w32toInteger itvl)), ("peers", (bencodePeers peers))]

bencodePeers :: [Peer] -> BE.BEncode
bencodePeers peers = BE.BList (map bencodePeer peers)
 where
  bencodePeer :: Peer -> BE.BEncode
  bencodePeer peer =
    let (SockAddrInet p h) = peerAddr peer
        (a, b, c, d)       = hostAddressToTuple h
        ip = fromStrict $ B8.intersperse '.' $ B.pack [a, b, c, d]
        port               = (read . show) p :: Word32
    in  BE.BDict $ M.fromList
          [ ("peer_id", (BE.BString $ runPut . put $ peerID peer))
          , ("ip"     , (BE.BString ip))
          , ("port"   , (w32toInteger port))
          ]

bencodeScrape :: ScrapeRes -> BE.BEncode
bencodeScrape sr = BE.BDict $ M.fromList
  [ ("complete"  , (w32toInteger (srSeeders sr)))
  , ("downloaded", (w32toInteger (srCompletions sr)))
  , ("incomplete", (w32toInteger (srLeechers sr)))
  ]

bencodeScrapes :: [(Infohash, ScrapeRes)] -> B8.ByteString
bencodeScrapes srs =
  let fileDictList = map
        (\(ih, sr) -> ((B8.unpack . w160toBString) ih, bencodeScrape sr))
        srs
      fileDict = (BE.BDict . M.fromList) fileDictList
  in  (toStrict . BE.bPack . BE.BDict . M.fromList) [("files", fileDict)]

emptyScrapeRes :: ScrapeRes
emptyScrapeRes = ScrapeRes { srSeeders = 0, srCompletions = 0, srLeechers = 0 }

isRFC1918 :: Word32 -> Bool
isRFC1918 addr =
  addr
    .&. 0xff000000
    ==  0x0a000000
    ||  addr
    .&. 0xfff00000
    ==  0xac100000
    ||  addr
    .&. 0xffff0000
    ==  0xc0a80000
