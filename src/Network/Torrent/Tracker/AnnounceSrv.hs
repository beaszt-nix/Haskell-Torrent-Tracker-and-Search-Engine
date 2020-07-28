{-# LANGUAGE BangPatterns, TupleSections, BlockArguments #-}

module Network.Torrent.Tracker.AnnounceSrv
  ( AnnounceConf(..)
  , AnnounceEnv(..)
  , AnnounceST(..)
  , AnnounceT(..)
  , IPVer(..)
  , defConf
  , emptyAnnounceST
  , hdlAnnounce
  , handleScrape
  , pruneQ
  )
where

import           Network.Torrent.Tracker.AnnounceReqTypes
import           Network.Torrent.Tracker.PeerAccess
import           Control.Concurrent.MVar
import           Control.Monad           hiding ( forM
                                                , mapM
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Int
import           Data.List                      ( foldl' )
import           Data.Maybe
import           Data.Time.Clock
import           Data.Traversable
import           Data.Word
import           Network.Socket
import qualified Data.ByteString               as B
import qualified Data.Map                      as M
import qualified Data.Set                      as S

type ActivePeer = M.Map PeerID UTCTime
type ActiveQueue = S.Set (UTCTime, Maybe PeerID)

data AnnounceST  = AnnounceST {
        activeHash :: MVar (M.Map Infohash HashRec),
        prLastSeen :: MVar (M.Map Infohash (MVar ActivePeer)),
        prActiveQueue :: MVar (M.Map Infohash (MVar ActiveQueue))
}

emptyAnnounceST :: IO AnnounceST
emptyAnnounceST = do
  actHash <- newMVar M.empty
  pls     <- newMVar M.empty
  paq     <- newMVar M.empty
  return AnnounceST { activeHash    = actHash
                    , prLastSeen    = pls
                    , prActiveQueue = paq
                    }

type Second = Int32

data AnnounceConf = AnnounceConf {
    ancInterval :: ! Second,
    ancMaxPeer :: ! Word32,
    ancDefPeer :: ! Word32,
    ancIdleTimeOut :: ! Second,
    ancAddrs :: [(String, String)]
}

defConf :: AnnounceConf
defConf = AnnounceConf
  { ancInterval    = 120
  , ancMaxPeer     = 50
  , ancDefPeer     = 30
  , ancIdleTimeOut = 360
  , ancAddrs       = [ ("0.0.0.0", "6969")
                     , --ipv4
                       ("::"     , "6970") --ipv6
                     ]
  }

-- Environment Data to store.
data AnnounceEnv = AnnounceEnv {
    anSt   :: AnnounceST,
    anConf :: AnnounceConf
}

type AnnounceT = ReaderT AnnounceEnv IO

getState :: AnnounceT AnnounceST
getState = asks anSt

getConf :: AnnounceT AnnounceConf
getConf = asks anConf

pruneQ :: AnnounceT ()
pruneQ = do
  now     <- liftIO getCurrentTime
  st      <- getState
  hrMap   <- liftIO $ takeMVar (activeHash st)
  lstSeen <- liftIO $ takeMVar (prLastSeen st)
  queue   <- liftIO $ takeMVar (prActiveQueue st)

  liftIO $ putMVar (activeHash st) hrMap
  liftIO $ putMVar (prLastSeen st) lstSeen
  liftIO $ putMVar (prActiveQueue st) queue

  forM_ (M.assocs lstSeen) $ \(hash, hashActivityM) -> do
    let hashQM = queue M.! hash
    let hr     = hrMap M.! hash
    pruneHQ now hashActivityM hashQM hr

pruneHQ
  :: UTCTime -> MVar ActivePeer -> MVar ActiveQueue -> HashRec -> AnnounceT ()
pruneHQ now hashActivityM hashQM hr = do
  activity <- liftIO $ takeMVar hashActivityM
  queue    <- liftIO $ takeMVar hashQM
  timeout  <- liftM ancIdleTimeOut getConf
  let old_now       = addUTCTime (fromIntegral $ negate timeout) now
  let (old, queue') = S.split (old_now, Nothing) queue
  let activity' =
        foldl' (flip (M.delete . fromJust . snd)) activity (S.elems old)
  liftIO $ putMVar hashActivityM activity'
  liftIO $ putMVar hashQM queue'
  liftIO $ forM_ [hrInet4 hr, hrInet6 hr] $ \phrM -> do
    phr <- takeMVar phrM
    putMVar phrM $ phr { phrSeeders  = cleanUp old (phrSeeders phr)
                       , phrLeechers = cleanUp old (phrLeechers phr)
                       }

cleanUp :: ActiveQueue -> RdmPeerList -> RdmPeerList
cleanUp old rpl = foldl' (flip (removePID . fromJust . snd)) rpl (S.elems old)

hdlAnnounce :: AnnounceReq -> AnnounceT AnnounceRes
hdlAnnounce an = do
  let peer = anPeer an
      hash = anInfoHash an
  st <- getState
  hr <- liftIO $ getHashRec st hash
  liftIO $ updateActivity st hash peer
  let phr = getPtclHashRec peer hr
      peerGetter | anEvent an == Just Completed = \count phr -> return []
                 | isSeeder an                  = getLeech
                 | otherwise                    = getPeers
  maxPeers <- liftM ancMaxPeer getConf
  defPeers <- liftM ancDefPeer getConf
  let peerWanted = case anEvent an of
        Just Completed -> 0
        otherwise      -> fromMaybe defPeers (anWant an)
      peerCount = min maxPeers peerWanted
  peers    <- liftIO $ peerGetter (fromIntegral peerCount) phr
  interval <- liftM ancInterval getConf
  addOrRemovePeer an
  (nSeed, nLeech, _) <- liftIO $ getPeerCount phr
  return PeerList { plInterval = fromIntegral interval
                  , plSeeders  = Just nSeed
                  , plLeechers = Just nLeech
                  , plPeers    = peers
                  }

updateActivity :: AnnounceST -> Infohash -> Peer -> IO ()
updateActivity st hash peer = do
  lastSeenMap <- takeMVar (prLastSeen st)
  qMap        <- takeMVar (prActiveQueue st)
  case M.lookup hash lastSeenMap of
    Nothing -> do
      nLastSeen <- newMVar M.empty
      nQueue    <- newMVar S.empty
      putMVar (prLastSeen st) $ M.insert hash nLastSeen lastSeenMap
      putMVar (prActiveQueue st) $ M.insert hash nQueue qMap
      updateHashActivity peer nLastSeen nQueue
    Just lastSeen -> do
      putMVar (prLastSeen st)    lastSeenMap
      putMVar (prActiveQueue st) qMap
      let activityQueue = qMap M.! hash
      updateHashActivity peer lastSeen activityQueue
 where
  updateHashActivity peer lastSeenM qM = do
    let pid = peerID peer
    now      <- getCurrentTime
    lastSeen <- takeMVar lastSeenM
    queue    <- takeMVar qM
    case M.lookup pid lastSeen of
      Nothing -> do
        putMVar lastSeenM $ M.insert pid now lastSeen
        putMVar qM $ S.insert (now, Just pid) queue
      Just oldNow -> do
        putMVar lastSeenM $ M.insert pid now lastSeen
        putMVar qM $ S.insert (now, Just pid) $ S.delete (oldNow, Just pid)
                                                         queue

getHashRec :: AnnounceST -> Infohash -> IO HashRec
getHashRec st hash = do
  hrMap <- takeMVar (activeHash st)
  case M.lookup hash hrMap of
    Just hr -> do
      putMVar (activeHash st) hrMap
      return hr
    Nothing -> do
      hr <- emptyHashRec
      putMVar (activeHash st) $ M.insert hash hr hrMap
      return hr

getPtclHashRec :: Peer -> HashRec -> MVar PtclHashRec
getPtclHashRec peer hr = case peerAddr peer of
  SockAddrInet{}  -> hrInet4 hr
  SockAddrInet6{} -> hrInet6 hr
  otherwise       -> error "No socket support"

isSeeder :: AnnounceReq -> Bool
isSeeder an = anLeft an == 0

data PeerAction = Add | Shift | Remove

addOrRemovePeer :: AnnounceReq -> AnnounceT ()
addOrRemovePeer an = do
  let peer = anPeer an
      hash = anInfoHash an
  st <- getState
  case anEvent an of
    Nothing        -> addAnnounce st an
    Just Started   -> addAnnounce st an
    Just Completed -> shiftAnnounce st an
    Just Stopped   -> removeAnnounce st an
 where
  addAnnounce :: AnnounceST -> AnnounceReq -> AnnounceT ()
  addAnnounce = updateAnnounce Add

  shiftAnnounce :: AnnounceST -> AnnounceReq -> AnnounceT ()
  shiftAnnounce = updateAnnounce Shift

  removeAnnounce :: AnnounceST -> AnnounceReq -> AnnounceT ()
  removeAnnounce = updateAnnounce Remove

  updateAnnounce :: PeerAction -> AnnounceST -> AnnounceReq -> AnnounceT ()
  updateAnnounce act st an = do
    let activeMapM = activeHash st
    activeMap <- liftIO $ takeMVar activeMapM
    hr        <- case M.lookup (anInfoHash an) activeMap of
      Nothing -> liftIO $ do
        newHr <- emptyHashRec
        putMVar activeMapM $ M.insert (anInfoHash an) newHr activeMap
        return newHr
      Just hr -> return hr
    liftIO $ putMVar activeMapM activeMap
    let phrM = getPtclHashRec (anPeer an) hr
    phr <- liftIO $ takeMVar phrM
    let
      (seedUpdater, leechUpdater, compInc) = case act of
        Shift ->
          (addPeer (anPeer an), return . removePID (peerID (anPeer an)), (+ 1))
        Add    -> makeUpdaters (addPeer (anPeer an)) an
        Remove -> makeUpdaters (return . removePID (peerID (anPeer an))) an
    liftIO $ do
      seeders'  <- seedUpdater (phrSeeders phr)
      leechers' <- leechUpdater (phrLeechers phr)
      putMVar phrM $ phr { phrSeeders       = seeders'
                         , phrLeechers      = leechers'
                         , phrCompleteCount = compInc (phrCompleteCount phr)
                         }
   where
    makeUpdaters
      :: (RdmPeerList -> IO RdmPeerList)
      -> AnnounceReq
      -> ( RdmPeerList -> IO RdmPeerList
         , RdmPeerList -> IO RdmPeerList
         , Word32 -> Word32
         )
    makeUpdaters mod an =
      if isSeeder an then (mod, return, id) else (return, mod, id)

data IPVer = IP4 | IP6

handleScrape :: IPVer -> [ScrapeReq] -> AnnounceT [ScrapeRes]
handleScrape ipVer hashes = do
  hashRec <- liftIO . readMVar =<< asks (activeHash . anSt)
  forM hashes $ \hash -> liftIO $ case M.lookup hash hashRec of
    Nothing -> return emptyScrapeRes
    Just hr -> do
      let mphr = case ipVer of
            IP4 -> hrInet4 hr
            IP6 -> hrInet6 hr
      (seeders, leechers, completed) <- getPeerCount mphr
      return ScrapeRes { srSeeders     = seeders
                       , srLeechers    = leechers
                       , srCompletions = completed
                       }
