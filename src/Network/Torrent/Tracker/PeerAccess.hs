{-# LANGUAGE BangPatterns  #-}

module Network.Torrent.Tracker.PeerAccess
  ( HashRec(..)
  , PtclHashRec(..)
  , RdmPeerList(..)
  , PeerMap(..)
  , PeerSet(..)
  , addPeer
  , getPeers
  , getLeech
  , emptyPL
  , emptyPtclHashRec
  , emptyHR
  , getPeersCount
  , hasPID
  , hasPeer
  , peerSize
  , removePID
  )
where

import           Network.Torrent.Tracker.AnnounceReqTypes
import           Control.Concurrent.MVar
import           Data.Monoid
import           Data.List
import           Control.Monad.Fail
import           Control.Monad.Primitive
import           Data.Word
import           Data.Foldable                  ( toList )
import           Control.Concurrent.MVar
import           Data.Hashable
import qualified Data.Sequence                 as Seq
import           System.Random.MWC
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS

type PeerMap = HM.HashMap PeerID Peer
type PeerSet = HS.HashSet PeerID

data RdmPeerList = RdmPeerList {
                  plCurrent :: PeerMap,
                  plNext :: PeerMap,
                  plPeerL :: PeerSet
                } deriving Show

sample :: (PrimMonad m, MonadFail m) => [a] -> Int -> Gen (PrimState m) -> m [a]
sample ys size = go 0 (l - 1) (Seq.fromList ys) where
  l = length ys
  go !n !i xs g
    | n >= size = return $! (toList . Seq.drop (l - size)) xs
    | otherwise = do
      j <- uniformR (0, i) g
      let toI  = xs `Seq.index` j
          toJ  = xs `Seq.index` i
          next = (Seq.update i toI . Seq.update j toJ) xs
      go (n + 1) (i - 1) next g

addPeer :: (PrimMonad m, MonadFail m) => Peer -> RdmPeerList -> m RdmPeerList
addPeer p pl = do
  let nexM  = plNext pl
      peer  = plPeerL pl
      pid   = peerID p
      nexM' = HM.insert pid p nexM
  return $ pl { plNext = nexM', plPeerL = HS.insert pid peer }

getUptoNPeers
  :: (PrimMonad m, MonadFail m) => Int -> RdmPeerList -> m ([Peer], RdmPeerList)
getUptoNPeers n pl = do
  let curMap = plCurrent pl
      nxMap  = plNext pl
      revS   = plPeerL pl
      size   = HM.size curMap
  if size >= n
    then do
      (left, right) <- splitM n curMap
      let nMap = HM.union left nxMap
          pl'  = RdmPeerList { plCurrent = right
                             , plNext    = nMap
                             , plPeerL   = revS <> (HS.fromList $ HM.keys left)
                             }
      return (HM.elems left, pl')
    else if HM.size nxMap <= (size - n)
      then return (concat [HM.elems curMap, HM.elems nxMap], pl)
      else do
        (l, r) <- splitM (size - n) nxMap
        let
          initRes = HM.elems curMap
          res     = concat [initRes, HM.elems l]
          nRev =
            revS
              <> (HS.fromList $ HM.keys l)
              <> (HS.fromList $ map peerID initRes)
          nCurMap = r <> curMap
          pl' = RdmPeerList { plCurrent = nCurMap, plNext = l, plPeerL = nRev }
        return (res, pl')
 where
  splitM
    :: (PrimMonad m, Ord a, Hashable a, MonadFail m)
    => Int
    -> HM.HashMap a v
    -> m (HM.HashMap a v, HM.HashMap a v)
  splitM n s = do
    let ss = HM.toList s
    l <- create >>= sample ss n
    let ls = HM.fromList l
    return (ls, HM.difference s ls)

hasPID :: PeerID -> RdmPeerList -> Bool
hasPID pid rpl = let pidS = plPeerL rpl in HS.member pid pidS

hasPeer :: Peer -> RdmPeerList -> Bool
hasPeer p = hasPID $ peerID p

removePID :: PeerID -> RdmPeerList -> RdmPeerList
removePID pid rpl =
  let curMap = plCurrent rpl
      nxMap  = plNext rpl
      pidS   = plPeerL rpl
  in  rpl { plCurrent = HM.delete pid curMap
          , plNext    = HM.delete pid curMap
          , plPeerL   = HS.delete pid pidS
          }
emptyPL :: RdmPeerList
emptyPL =
  RdmPeerList { plCurrent = HM.empty, plNext = HM.empty, plPeerL = HS.empty }

data PtclHashRec = PtclHashRec {
                    phrSeeders :: !RdmPeerList,
                    phrLeecher :: !RdmPeerList,
                    phrComplete :: !Word32 }

data HashRec = HashRec {
                  hrInet4 :: MVar PtclHashRec,
                  hrInet6 :: MVar PtclHashRec
               }

emptyPtclHashRec :: PtclHashRec
emptyPtclHashRec =
  PtclHashRec { phrSeeders = emptyPL, phrLeecher = emptyPL, phrComplete = 0 }

emptyHR :: IO HashRec
emptyHR = do
  phr4 <- newMVar emptyPtclHashRec
  phr6 <- newMVar emptyPtclHashRec
  return HashRec { hrInet4 = phr4, hrInet6 = phr6 }

getPeersGen :: Int -> Int -> MVar PtclHashRec -> IO [Peer]
getPeersGen ns nl mphr = do
  phr               <- takeMVar mphr
  (sPeerList, sRpl) <- getUptoNPeers ns $ phrSeeders phr
  (lPeerList, lRpl) <- getUptoNPeers nl $ phrLeecher phr
  putMVar mphr $ phr { phrSeeders = sRpl, phrLeecher = lRpl }
  return $ concat [sPeerList, lPeerList]

getPeers :: Int -> MVar PtclHashRec -> IO [Peer]
getPeers n mphr =
  let nSeed = div (n + 9) 10
      nLeec = n - nSeed
  in  getPeersGen nSeed nLeec mphr

getLeech :: Int -> MVar PtclHashRec -> IO [Peer]
getLeech = getPeersGen 0

getPeersCount :: MVar PtclHashRec -> IO (Word32, Word32, Word32)
getPeersCount mphr = do
  phr <- readMVar mphr
  return (f $ phrSeeders phr, f $ phrLeecher phr, phrComplete phr)
  where f = fromIntegral . HS.size . plPeerL

peerSize :: RdmPeerList -> Int
peerSize = HS.size . plPeerL
