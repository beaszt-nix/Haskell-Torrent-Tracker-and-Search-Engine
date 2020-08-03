{-# LANGUAGE BangPatterns, TupleSections, BlockArguments #-}

module Network.Torrent.Tracker.AnnounceSrv where

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
import           Data.Hashable
import           Data.Word
import           Network.Socket
import           Data.Time.Calendar             ( toModifiedJulianDay )
import qualified Data.ByteString               as B
import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as S

type ActivePeer = M.HashMap PeerID UTCTime
type ActiveQueue = S.HashSet (UTCTime, PeerID)

instance Hashable UTCTime where
  hashWithSalt salt (UTCTime d dt) =
    salt
      `hashWithSalt` (toModifiedJulianDay d)
      `hashWithSalt` (diffTimeToPicoseconds dt)

data AnnounceST  = AnnounceST {
        activeHash :: MVar (M.HashMap Infohash HashRec),
        prLastSeen :: MVar (M.HashMap Infohash (MVar ActivePeer)),
        prActiveQueue :: MVar (M.HashMap Infohash (MVar ActiveQueue))
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
defConf = AnnounceConf { ancInterval    = 120
                       , ancMaxPeer     = 50
                       , ancDefPeer     = 30
                       , ancIdleTimeOut = 360
                       , ancAddrs       = [("0.0.0.0", "6969"), ("::", "6970")]
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

pruneQueue :: AnnounceT ()
pruneQueue = do
  now         <- liftIO getCurrentTime
  st          <- getState
  hrMap       <- liftIO $ takeMVar $ activeHash st
  lastSeen    <- liftIO $ takeMVar $ prLastSeen st
  activeQueue <- liftIO $ takeMVar $ prActiveQueue st

  forM_ (M.toList lastSeen) $ \(hash, hashActivityMVar) -> do
    let activeQueueMvar = activeQueue M.! hash
        hashRecord      = hrMap M.! hash
    pruneHashQueue now hashActivityMVar activeQueueMvar hashRecord

  liftIO $ putMVar (activeHash st) hrMap
  liftIO $ putMVar (prLastSeen st) lastSeen
  liftIO $ putMVar (prActiveQueue st) activeQueue
 where
  pruneHashQueue
    :: UTCTime -> MVar ActivePeer -> MVar ActiveQueue -> HashRec -> AnnounceT ()
  pruneHashQueue now hashActiveMVar activeQueueMvar hashRec = do
    activityHashes <- liftIO $ takeMVar $ hashActiveMVar
    activeQueue    <- liftIO $ takeMVar $ activeQueueMvar
    timout         <- liftM ancIdleTimeOut getConf

    let
      oldNow = addUTCTime (fromIntegral $ negate timout) now
      old =
        S.map (\x -> snd x) . S.filter (\x -> (fst x) < oldNow) $ activeQueue
      activeHashes =
        M.filterWithKey (\k _ -> not $ S.member k old) activityHashes

    liftIO $ putMVar hashActiveMVar activeHashes
    liftIO $ putMVar activeQueueMvar $ S.filter
      (\(_, v) -> not $ S.member v old)
      activeQueue
    liftIO $ forM_ [hrInet4 hashRec, hrInet6 hashRec] $ \phrM -> do
      phr <- takeMVar phrM
      putMVar phrM $ phr { phrSeeders = clean old (phrSeeders phr)
                         , phrLeecher = clean old (phrLeecher phr)
                         }
    where clean old rdm = S.foldl' (flip removePID) rdm old

getHashRec :: AnnounceST -> Infohash -> IO HashRec
getHashRec st hash = do
  activeHashes <- liftIO $ takeMVar (activeHash st)
  case M.lookup hash activeHashes of
    Just hr -> do
      putMVar (activeHash st) activeHashes
      return hr
    Nothing -> do
      hr <- emptyHR
      putMVar (activeHash st) $ M.insert hash hr activeHashes
      return hr


updateHashActivity :: Peer -> MVar ActivePeer -> MVar ActiveQueue -> IO ()
updateHashActivity peer actPeerM actQueueM = do
  let pid = peerID peer
  now      <- getCurrentTime
  lastSeen <- takeMVar actPeerM
  actQueue <- takeMVar actQueueM
  case M.lookup pid lastSeen of
    Nothing -> do
      putMVar actPeerM $ M.insert pid now lastSeen
      putMVar actQueueM $ S.insert (now, pid) actQueue
    Just on -> do
      putMVar actPeerM $ M.insert pid now lastSeen
      putMVar actQueueM $ S.insert (now, pid) $ S.delete (on, pid) actQueue

updateActivity :: AnnounceST -> Infohash -> Peer -> IO ()
updateActivity st hash peer = do
  lastSeen    <- takeMVar (prLastSeen st)
  activeQueue <- takeMVar (prActiveQueue st)
  case M.lookup hash lastSeen of
    Nothing -> do
      newLastSeen <- newMVar M.empty
      newQueue    <- newMVar S.empty
      putMVar (prLastSeen st) $ M.insert hash newLastSeen lastSeen
      putMVar (prActiveQueue st) $ M.insert hash newQueue activeQueue
      updateHashActivity peer newLastSeen newQueue
    Just ls -> do
      putMVar (prLastSeen st)    lastSeen
      putMVar (prActiveQueue st) activeQueue
      let activityQueue = activeQueue M.! hash
      updateHashActivity peer ls activityQueue

getPHR :: Peer -> HashRec -> MVar PtclHashRec
getPHR peer hr = case peerAddr peer of
  SockAddrInet{}  -> hrInet4 hr
  SockAddrInet6{} -> hrInet6 hr
  otherwise       -> error "Socket Not supported"

peerGetter :: AnnounceReq -> Int -> MVar PtclHashRec -> IO [Peer]
peerGetter an | anEvent an == Just Completed = \_ _ -> return []
              | isSeeder an                  = getLeech
              | otherwise                    = getPeers

isSeeder :: AnnounceReq -> Bool
isSeeder = (==) 0 . anLeft

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
        newHr <- emptyHR
        putMVar activeMapM $ M.insert (anInfoHash an) newHr activeMap
        return newHr
      Just hr -> return hr
    liftIO $ putMVar activeMapM activeMap
    let phrM = getPHR (anPeer an) hr
    phr <- liftIO $ takeMVar phrM
    let
      (seedUpdater, leechUpdater, compInc) = case act of
        Shift ->
          (addPeer (anPeer an), return . removePID (peerID (anPeer an)), (+ 1))
        Add    -> makeUpdaters (addPeer (anPeer an)) an
        Remove -> makeUpdaters (return . removePID (peerID (anPeer an))) an
    liftIO $ do
      seeders'  <- seedUpdater (phrSeeders phr)
      leechers' <- leechUpdater (phrLeecher phr)
      putMVar phrM $ phr { phrSeeders  = seeders'
                         , phrLeecher  = leechers'
                         , phrComplete = compInc (phrComplete phr)
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

handleAnnounce :: AnnounceReq -> AnnounceT AnnounceRes
handleAnnounce an = do
  let peer = anPeer an
      hash = anInfoHash an
  st <- getState
  hr <- liftIO $ getHashRec st hash
  liftIO $ updateActivity st hash peer

  let phr = getPHR peer hr

  maxPeer <- liftM ancMaxPeer getConf
  defPeer <- liftM ancDefPeer getConf

  let numWant = case anEvent an of
        Just Completed -> 0
        otherwise      -> fromMaybe defPeer (anWant an)
      pc = min maxPeer numWant

  peers    <- liftIO $ peerGetter an (fromIntegral pc) phr
  interval <- liftM ancInterval getConf
  addOrRemovePeer an
  (ns, nl, _) <- liftIO $ getPeersCount phr
  return PeerList { plInterval = fromIntegral interval
                  , plSeeders  = Just ns
                  , plLeechers = Just nl
                  , plPeers    = peers
                  }


data IPV = IP4 | IP6

handleScrape :: IPV -> [ScrapeReq] -> AnnounceT [ScrapeRes]
handleScrape ipV hashes = do
  hashRec <- liftIO . readMVar =<< asks (activeHash . anSt)
  forM hashes $ \hash -> liftIO $ case M.lookup hash hashRec of
    Nothing -> return emptyScrapeRes
    Just hr -> do
      let mphr = case ipV of
            IP4 -> hrInet4 hr
            IP6 -> hrInet6 hr
      (s, l, c) <- getPeersCount mphr
      return ScrapeRes { srSeeders = s, srLeechers = l, srCompletions = c }
