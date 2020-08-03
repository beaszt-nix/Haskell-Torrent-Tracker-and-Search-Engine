{-# LANGUAGE BangPatterns #-}

module Network.Torrent.Tracker.AnnounceUDP where
import           Network.Torrent.Tracker.AnnounceReqTypes
import           Network.Torrent.Tracker.AnnounceSrv

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Crypto.Saltine.Core.Hash

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Digest.SHA1
import           Data.Bits
import           Data.Maybe
import           Data.Word

import           Network.Socket
import           Network.Socket.ByteString

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
                                                ( pack )
import qualified Data.ByteString.Lazy          as BL


data UdpEnv = UdpEnv {
                anEnv :: AnnounceEnv,
                currKey :: MVar ShorthashKey,
                prevKey :: MVar ShorthashKey
              }
makeUdpEnv :: AnnounceEnv -> IO UdpEnv
makeUdpEnv anEnv = do
  currKeyM <- newMVar =<< newShorthashKey
  prevKeyM <- newMVar =<< newShorthashKey
  return UdpEnv { anEnv = anEnv, currKey = currKeyM, prevKey = prevKeyM }

type UdpT a = ReaderT UdpEnv IO a
type ConnectionID = Word64
type TransactionID = Word32

cycleKeys :: UdpT ()
cycleKeys = do
  currK <- liftIO . takeMVar =<< asks currKey
  prevK <- liftIO . takeMVar =<< asks prevKey
  newK  <- liftIO newShorthashKey
  liftIO . (`putMVar` newK) =<< asks currKey
  liftIO . (`putMVar` currK) =<< asks prevKey


liftAnnounceT :: AnnounceT a -> UdpT a
liftAnnounceT = ReaderT . (. anEnv) . runReaderT

data RequestHeader = RequestHeader {
                        reqConnectionID :: ! ConnectionID,
                        reqAction :: ! Word32,
                        reqTransactionID :: !TransactionID
                     }

data ResponseHeader = ResponseHeader {
                        resAction :: !Word32
                      , resTransactionID :: !TransactionID
                     }

makeResponseHeader :: RequestHeader -> ResponseHeader
makeResponseHeader reqH = ResponseHeader
  { resAction        = reqAction reqH
  , resTransactionID = reqTransactionID reqH
  }

makeErrorHeader :: RequestHeader -> ResponseHeader
makeErrorHeader reqH =
  ResponseHeader { resAction = 3, resTransactionID = reqTransactionID reqH }

instance Binary ResponseHeader where
  get = liftM2 ResponseHeader get get
  put rh = do
    put $ resAction rh
    put $ resTransactionID rh

instance Binary RequestHeader where
  get = liftM3 RequestHeader get get get
  put rh = do
    put $ reqConnectionID rh
    put $ reqAction rh
    put $ reqTransactionID rh

instance Binary PortNumber where
  get = liftM fromIntegral getWord16be
  put pn = putWord16be (fromIntegral pn)

instance Binary ScrapeRes where
  get = do
    l <- get
    s <- get
    return ScrapeRes { srSeeders = s, srCompletions = 0, srLeechers = l }

  put sr = do
    put $ srLeechers sr
    put $ srSeeders sr

connectionHashString :: SockAddr -> B.ByteString
connectionHashString sock = BL.toStrict $ case sock of
  SockAddrInet p h      -> encode h `BL.append` encode p
  SockAddrInet6 p _ h _ -> encode h `BL.append` encode p

fetchCurrKey :: UdpT ShorthashKey
fetchCurrKey = liftIO . readMVar =<< asks currKey
fetchPrevKey :: UdpT ShorthashKey
fetchPrevKey = liftIO . readMVar =<< asks prevKey

isValidConnId :: SockAddr -> ConnectionID -> UdpT Bool
isValidConnId sock connId = do
  let connStr = connectionHashString sock
  currKey <- fetchCurrKey
  prevKey <- fetchPrevKey
  let currHash = decode $ BL.fromStrict $ shorthash currKey connStr
      prevHash = decode $ BL.fromStrict $ shorthash prevKey connStr
  return (connId == currHash || connId == prevHash)


handleConnect
  :: SockAddr -> RequestHeader -> UdpT (Maybe (ResponseHeader, ConnectionID))
handleConnect sock rh = do
  let connID = reqConnectionID rh
  case connID of
    0x41727101980 -> do
      let connStr = connectionHashString sock
      key <- fetchCurrKey
      let
        resH = ResponseHeader { resAction        = 0
                              , resTransactionID = reqTransactionID rh
                              }
        connID = decode $ BL.fromStrict $ shorthash key connStr
      return $ Just (resH, connID)
    _ -> return Nothing

getUdpAnnounce4 :: Get AnnounceReq
getUdpAnnounce4 = getUdpAnnounceGen get SockAddrInet

getUdpAnnounce6 :: Get AnnounceReq
getUdpAnnounce6 = getUdpAnnounceGen get $ \p a -> SockAddrInet6 p 0 a 0

getUdpAnnounceGen :: Get a -> (PortNumber -> a -> SockAddr) -> Get AnnounceReq
getUdpAnnounceGen getAddr buildSock = do
  ih          <- get :: Get Infohash
  pid         <- get :: Get PeerID
  bdownloaded <- get :: Get Word64
  bleft       <- get :: Get Word64
  buploaded   <- get :: Get Word64
  eventCode   <- get :: Get Word32
  let event = case eventCode of
        1 -> Just Completed
        2 -> Just Started
        3 -> Just Stopped
        _ -> Nothing
  ipaddr   <- getAddr
  _key     <- get :: Get Word32
  wantCode <- get :: Get Word32
  let want = case wantCode of
        0xffffffff -> Nothing
        x          -> Just x
  port <- get :: Get PortNumber
  return AnnounceReq
    { anInfoHash   = ih
    , anPeer       = Peer { peerID = pid, peerAddr = buildSock port ipaddr }
    , anUploaded   = buploaded
    , anDownloaded = bdownloaded
    , anLeft       = bleft
    , anEvent      = event
    , anWant       = want
    }

packPeers4 :: [Peer] -> Put
packPeers4 = mapM_ packPeer4
 where
  packPeer4 p = case peerAddr p of
    SockAddrInet port addr -> do
      putWord32host addr
      put port
    _ -> return ()

packPeers6 :: [Peer] -> Put
packPeers6 = mapM_ packPeer6
 where
  packPeer6 p = case peerAddr p of
    SockAddrInet6 port _ (a1, a2, a3, a4) _ -> do
      putWord32host a1
      putWord32host a2
      putWord32host a3
      putWord32host a4
      put port
    _ -> return ()


packAnnounceResponse4 :: AnnounceRes -> Put
packAnnounceResponse4 = packAnnounceResponseGen packPeers4

packAnnounceResponse6 :: AnnounceRes -> Put
packAnnounceResponse6 = packAnnounceResponseGen packPeers6

packAnnounceResponseGen :: ([Peer] -> Put) -> AnnounceRes -> Put
packAnnounceResponseGen packPeers ar = case ar of
  Failure message -> put message
  PeerList { plInterval = ival, plSeeders = ns, plLeechers = nl, plPeers = peers }
    -> do
      put ival
      put (fromMaybe 0 nl)
      put (fromMaybe 0 ns)
      packPeers peers

handleUdpRequest :: Socket -> SockAddr -> B.ByteString -> UdpT ()
handleUdpRequest sock addr msg = case runGetOrFail get (BL.fromStrict msg) of
  Left  _             -> return ()
  Right (msg', _, rh) -> do
    let (announceAction, announceGetter, announcePack, ipVersion) =
          case addr of
            SockAddrInet{}  -> (1, getUdpAnnounce4, packAnnounceResponse4, IP4)
            SockAddrInet6{} -> (4, getUdpAnnounce6, packAnnounceResponse6, IP6)
    case reqAction rh of
      0 -> do
        mresp <- handleConnect addr rh
        case mresp of
          Nothing                   -> return ()
          Just (respHeader, connId) -> do
            let p = put respHeader >> put connId
            writeResponse p
      x | x == announceAction ->
        whenValid addr rh $ whenParses rh announceGetter msg' $ \an -> do
          let an' = case addr of
                SockAddrInet6 _ _ addr6 _ -> updateAddr6 addr6 an
                SockAddrInet port addr4   -> if isRFC1918 addr4
                  then case peerAddr $ anPeer an of
                    SockAddrInet _ 0 -> updateAddr4 addr4 an
                    _                -> an
                  else updateAddr4 addr4 an
          anResp <- liftAnnounceT $ handleAnnounce an'
          writeResponse $ put (makeResponseHeader rh) >> announcePack anResp
      2 -> whenValid addr rh $ case BL.length msg' `divMod` 20 of
        (n, 0) ->
          whenParses rh (replicateM (fromIntegral n) get) msg' $ \sreqs -> do
            sresps <- liftAnnounceT $ handleScrape ipVersion sreqs
            writeResponse (put (makeResponseHeader rh) >> mapM_ put sresps)
        _ -> do
          let p = put (makeErrorHeader rh)
                >> put (B8.pack "Scrape should be a multiple of 20 bytes.")
          writeResponse p
      _ -> do
        let
          p = put (makeErrorHeader rh)
            >> put (B8.pack "currently unsupported request.")
        writeResponse p
 where
  whenParses
    :: RequestHeader -> Get a -> BL.ByteString -> (a -> UdpT ()) -> UdpT ()
  whenParses rh getter msg action = case runGetOrFail getter msg of
    Left _ -> writeResponse $ put (makeErrorHeader rh) >> put
      (B8.pack "error parsing request.")
    Right (_, _, a) -> action a

  whenValid :: SockAddr -> RequestHeader -> UdpT () -> UdpT ()
  whenValid addr rh action = do
    validity <- isValidConnId addr (reqConnectionID rh)
    when validity action
  writeResponse :: Put -> UdpT ()
  writeResponse p = do
    let respMsg = BL.toStrict $ runPut p
    liftIO $ sendAllTo sock respMsg addr
  updateAddr6 addr6 an =
    let peerAddr' = case peerAddr $ anPeer an of
          SockAddrInet6 port flow _ scopeId ->
            SockAddrInet6 port flow addr6 scopeId
    in  an { anPeer = (anPeer an) { peerAddr = peerAddr' } }
  updateAddr4 addr4 an =
    let peerAddr' = case peerAddr $ anPeer an of
          SockAddrInet port _ -> SockAddrInet port addr4
    in  an { anPeer = (anPeer an) { peerAddr = peerAddr' } }
