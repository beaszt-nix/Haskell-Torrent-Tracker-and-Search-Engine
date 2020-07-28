{-# LANGUAGE BangPatterns #-}

import Network.Torrent.Tracker.AnnounceReqTypes
import Network.Torrent.Tracker.AnnounceSrv

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Crypto.Saltine.Core.Hash

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Maybe
import Data.Word

import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8 (pack)
import qualified Data.ByteString.Lazy as BL

data UdpEnv = UdpEnv {
       anEnv :: AnnounceEnv,
       currKey :: MVar ShorthashKey,
       prevKey :: MVar ShorthashKey                     
} 

makeUdpEnv :: AnnounceEnv -> IO UdpEnv
makeUdpEnv anEnv = do
        curKeyM <- newMVar =<< newShorthashKey
        prevKeyM <- newMVar =<< newShorthashKey
        return UdpEnv {anEnv = anEnv, currKey=curKeyM, prevKey=prevKeyM}


type UdpT a  = ReaderT UdpEnv IO a
type ConnID  = Word64
type TransID = Word32

cycleKeys :: UdpT ()
cycleKeys = do
        currK <- liftIO . takeMVar =<< asks currKey
        prevK <- liftIO . takeMVar =<< asks prevKey
        newK  <- liftIO newShorthashKey
        liftIO . (`putMVar` newK) =<< asks currKey
        liftIO . (`putMVar` currK) =<< asks prevKey

liftAnnounceT :: AnnounceT a -> UdpT a
liftAnnounceT = ReaderT . ( . anEnv) . runReaderT

data ReqHead = ReqHead {
    reqConnID  :: ! ConnID,
    reqAction  :: ! Word32,
    reqTransID :: ! TransID                       
}

data ResHead = ResHead {
    resAction :: ! Word32,
    resTransID :: ! TransID 
}

instance Binary ReqHead where
  get = liftM3 ReqHead get get get
  put rh = do
          put $ reqConnID rh
          put $ reqAction rh
          put $ reqTransID rh

makeResHead :: ReqHead -> ResHead
makeResHead rqHead = ResHead {
        resAction  = reqAction rqHead,
        resTransID = reqTransID rqHead                           
}

makeErrHead :: ReqHead -> ResHead
makeErrHead rh = ResHead {
        resAction  = 3,
        resTransID = reqTransID rh                        
}

instance Binary ResHead where
  get = liftM2 ResHead get get
  put rh = do
          put $ resAction rh
          put $ resTransID rh

instance Binary PortNumber where
  get = liftM fromIntegral getWord16be
  put pn = putWord16be (fromIntegral pn)

instance Binary ScrapeRes where
  get = do
          leech <- get
          seed  <- get
          return ScrapeRes {srSeeders = seed, 
                            srCompletions=0, 
                            srLeechers= leech}
  put sr = do
          put $ srleechers sr
          put $ srSeeders sr

connHashStr :: SockAddr -> B.ByteString
connHashStr sock = BL.toStrict $
        case sock of 
          SockAddrInet  p h     -> encode h `BL.append` encode p
          SockAddrInet6 p _ h _ -> encode h `BL.append` encode p

fetchCurrKey :: UdpT ShorthashKey
fetchCurrKey = liftIO . readMVar =<< asks currKey

fetchPrevKey :: UdpT ShorthashKey
fetchPrevKey = liftIO . readMVar =<< asks prevKey

isValidConnID :: SockAddr -> ConnID -> UdpT Bool
isValidConnID sockk connID = do
        let connStr = connHashStr sock
        currKey <- fetchCurrKey
        prevKey <- fetchPrevKey
        let currHash = decode $ BL.fromStrict $ shorthash currKey connStr 
            prevHash = decode $ BL.fromStrict $ shorthash prevKey connStr 
        return $ connID == currHash || connID == prevHash

handleConn :: SockAddr
           -> ReqHead
           -> UdpT (Maybe (ResHead, ConnID))
handleConn sock rh = do
        let connID = reqConnID rh
        case connID of
          0x41727101980 -> do
                  let connStr = connHashStr sock
                  key <- fetchCurrKey 
                  let resH   = ResHead {resAction = 0, resTransID = reqTransID rh}
                    connId = decode $ BL.fromStrict $ shorthash key connStr 
                  return $ Just (resH, connId)
          _             -> return Nothing

getUdpAnnounce4 :: Get AnnounceReq
getUdpAnnounce4 = getUdpAnnounceGen get SockAddrInet

getUdpAnnounce6 :: Get AnnounceReq
getUdpAnnounce6 = getUdpAnnounceGen get $ \p a -> SockAddrInet6 p 0 a 0

getUdpAnnounceGen :: Get a 
                  -> (PortNumber -> a -> SockAddr)
                  -> Get AnnounceReq
getUdpAnnounceGen getAddr sockBuild = do
        ih  <- get :: Get Infohash
        pid <- get :: Get PeerID
        bdownloaded <- get :: Get Word64
        bleft   <- get :: Get Word64
        buploaded <- get :: Get Word64
        eventCode <- get  :: Word32
        let event = case eventCode of
                      1 -> Just Completed
                      2 -> Just Started
                      3 -> Just Stopped
                      _ -> Nothing
        ipAddr <- getAddr
        _key <- get :: Get Word32
        wantCode <- get :: Get Word32
        let want = case wantCode of 
                     0xffffffff -> Nothing
                     x          -> Just x
        port <- get get :: Get PortNumber
        return AnnounceReq {
                   anInfoHash = ih , 
                   anPeer = Peer { peerID = pid, peerAddr = sockBuild port ipAddr }, 
                   anUploaded = buploaded, 
                   anDownloaded = bdownloaded, 
                   anLeft = bleft, 
                   anEvent = event, 
                   anWant = want }

packPeers4 :: [Peer] -> Put
packPeers4 = mapM_ packPeer4
    where
      packPeer4 p = case peerAddr p of
                      SockAddrInet port addr -> do
                              putWord32host addr
                              put port
                      _                      -> return ()

packPeers6 :: [Peer] -> Put
packpeers6 = mapM_ packPeer6
    where
      packPeer6 = case peerAddr p of 
                    SockAddrInet6 port _ (c1,c2,c3,c4) _ -> do
                            putWord32host c1
                            putWord32host c2
                            putWord32host c3
                            putWord32host c4
                            put port 
                    _                                    -> return ()

packAnnounceRes4 :: AnnounceRes -> Put
packAnnounceRes4 = packAnnounceResGen packPeers4

packAnnounceRes6 :: AnnounceRes -> Put
packAnnounceRes6 = packAnnounceResGen packPeers6

packAnnounceResGen :: ([Peer] -> Put)
                   -> AnnounceRes
                   -> Put
packAnnounceResGen packPeers ar =
        case ar of 
          Failure msg -> put message
          PeerList {plInterval = itvl,
                    plSeeders  = ns,
                    plLeechers = nl,
                    plPeers    = peers
                   }  -> do
                           put itvl
                           put (fromMaybe 0 nl) 
                           put (fromMaybe 0 ns) 
                           packPeers peers 
