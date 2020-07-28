{-# LANGUAGE BangPatterns  #-}

module Network.Torrent.Tracker.PeerAccess
  ( HashRec(..)
  , PtclHashRec(..)
  , RdmPeerList(..)
  , RdmPeerMap(..)
  , RevPeerMap(..)
  , addPeer
  , emptyHashRec
  , emptyPL
  , hasPeer
  , hasPID
  , getLeech
  , getUptoNPeer
  , getPeerCount
  , getPeers
  , removePID
  )
where

import           Network.Torrent.Tracker.AnnounceReqTypes
import           Control.Concurrent.MVar
import           Control.Monad.Random
import           Data.List
import           Data.Tuple
import           Data.Word
import qualified Data.Map                      as M

-- Reverse  map added for ease of deletion.
type RdmPeerMap = M.Map (Word32, Maybe PeerID) Peer
type RevPeerMap = M.Map (Maybe PeerID) Word32

data RdmPeerList   = RdmPeerList {
                rplCurrent :: RdmPeerMap,
                rplNext :: RdmPeerMap,
                rplReverse :: RevPeerMap
             } deriving Show

getUptoNPeer :: (MonadRandom m) => Int -> RdmPeerList -> m ([Peer], RdmPeerList)
getUptoNPeer n pl = do
  let curMap = rplCurrent pl
      nxMap  = rplNext pl
      revMap = rplReverse pl
      curLen = M.size curMap
  if curLen >= n
    then do
      let (left, right) = splitM n curMap
      let result        = M.elems left

      nAssoc <- mapM buildRandKey result
      let
        nMap = appendMap nxMap nAssoc
        nRev = updateRevMap nAssoc revMap
        pl' =
          RdmPeerList { rplCurrent = right, rplNext = nMap, rplReverse = nRev }
      return (result, pl')
    else if M.size nxMap <= (curLen - n)
      then do
        let curList = M.elems curMap
            nxList  = M.elems nxMap
            result  = curList ++ nxList
        return (result, pl)
      else do
        let initRes       = M.elems curMap
            (left, right) = splitM (curLen - n) nxMap
            result        = initRes ++ M.elems left
        nAssoc <- mapM buildRandKey $ M.elems left
        oAssoc <- mapM buildRandKey initRes
        let
          nCurMap = appendMap right oAssoc
          nMap    = M.fromList nAssoc
          nRev    = updateRevMap (oAssoc ++ nAssoc) revMap
          pl'     = RdmPeerList { rplCurrent = nCurMap
                                , rplNext    = nMap
                                , rplReverse = nRev
                                }
        return (result, pl')

 where
  splitM
    :: (Ord k)
    => Int
    -> M.Map (Word32, Maybe k) v
    -> (M.Map (Word32, Maybe k) v, M.Map (Word32, Maybe k) v)
  splitM n m = if M.size m > n
    then let ((sKey, _), _) = M.elemAt n m in M.split (sKey, Nothing) m
    else (m, M.empty)
  appendMap :: (Ord k) => M.Map k v -> [(k, v)] -> M.Map k v
  appendMap m l = M.union m (M.fromList l)
  updateRevMap :: [((Word32, Maybe PeerID), Peer)] -> RevPeerMap -> RevPeerMap
  updateRevMap kv revMap =
    let revKeys = map (swap . fst) kv in M.union revMap $ M.fromList revKeys
  buildRandKey :: (MonadRandom m) => Peer -> m ((Word32, Maybe PeerID), Peer)
  buildRandKey peer = do
    randKey <- getRandomR (minBound, maxBound)
    return ((randKey, Just (peerID peer)), peer)

addPeer :: (MonadRandom m) => Peer -> RdmPeerList -> m RdmPeerList
addPeer p rpl = do
  let nxMap  = rplNext rpl
      revMap = rplReverse rpl
      k      = Just (peerID p)
  case M.lookup k revMap of
    Nothing -> do
      r <- getRandom
      return $ rpl { rplNext    = M.insert (r, Just (peerID p)) p nxMap
                   , rplReverse = M.insert (Just (peerID p)) r revMap
                   }
    Just _ -> return rpl

hasPeer :: Peer -> RdmPeerList -> Bool
hasPeer p = hasPID (peerID p)

hasPID :: PeerID -> RdmPeerList -> Bool
hasPID pid rpl =
  let revMap = rplReverse rpl
      k      = Just pid
  in  M.member k revMap

removePID :: PeerID -> RdmPeerList -> RdmPeerList
removePID pid rpl =
  let curMap = rplCurrent rpl
      nxMap  = rplNext rpl
      revMap = rplReverse rpl
      key    = Just pid
  in  case M.lookup key revMap of
        Nothing -> rpl
        Just r  -> RdmPeerList { rplCurrent = M.delete (r, key) curMap
                               , rplNext    = M.delete (r, key) nxMap
                               , rplReverse = M.delete key revMap
                               }

emptyPL :: RdmPeerList
emptyPL =
  RdmPeerList { rplCurrent = M.empty, rplNext = M.empty, rplReverse = M.empty }

data HashRec = HashRec {
        hrInet4 :: MVar PtclHashRec,
        hrInet6 :: MVar PtclHashRec
}

emptyHashRec :: IO HashRec
emptyHashRec = do
  phr4 <- newMVar emptyPtclHashRec
  phr6 <- newMVar emptyPtclHashRec
  return HashRec { hrInet4 = phr4, hrInet6 = phr6 }

data PtclHashRec = PtclHashRec {
        phrSeeders :: ! RdmPeerList,
        phrLeechers :: ! RdmPeerList,
        phrCompleteCount :: !Word32
   }

emptyPtclHashRec :: PtclHashRec
emptyPtclHashRec = PtclHashRec { phrSeeders       = emptyPL
                               , phrLeechers      = emptyPL
                               , phrCompleteCount = 0
                               }

getPeers :: Int -> MVar PtclHashRec -> IO [Peer]
getPeers n mphr =
  let nSeed  = div (n + 9) 10
      nLeech = n - nSeed
  in  getPeersGen nSeed nLeech mphr

getLeech :: Int -> MVar PtclHashRec -> IO [Peer]
getLeech = getPeersGen 0

getPeersGen :: Int -> Int -> MVar PtclHashRec -> IO [Peer]
getPeersGen nSeed nLeech mphr = do
  gen1 <- newStdGen
  gen2 <- newStdGen
  phr  <- takeMVar mphr
  let count             = nSeed + nLeech
      seeds             = phrSeeders phr
      leeches           = phrLeechers phr
      avSeed            = (M.size . rplReverse) seeds
      avLeech           = (M.size . rplReverse) leeches
      (nSeed', nLeech') = if avSeed + avLeech <= count
        then (avSeed, avLeech)
        else
          let minSeed  = min avSeed nSeed
              minLeech = min avLeech nLeech
              sdBackF  = count - avLeech
              lcBackF  = count - avSeed
          in  (max minSeed sdBackF, max minLeech lcBackF)
  let (seedList , seed'   ) = evalRand (getUptoNPeer nSeed' seeds) gen1
      (leechList, leeches') = evalRand (getUptoNPeer nLeech' leeches) gen2
  putMVar mphr $ phr { phrSeeders = seed', phrLeechers = leeches' }
  return $ seedList ++ leechList

getPeerCount :: MVar PtclHashRec -> IO (Word32, Word32, Word32)
getPeerCount mphr = do
  phr <- readMVar mphr
  let seeds    = phrSeeders phr
      leeches  = phrLeechers phr
      complete = phrCompleteCount phr
  return
    ( fromIntegral $ (M.size . rplReverse) seeds
    , fromIntegral $ (M.size . rplReverse) leeches
    , complete
    )
