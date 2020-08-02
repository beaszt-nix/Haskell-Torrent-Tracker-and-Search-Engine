module Main where

import           Control.Monad
import           Control.Monad.Random
import           Data.List                      ( nub )
import           Data.Word                      ( Word16 )
import           Network.Torrent.Tracker.AnnounceReqTypes
import           Network.Torrent.Tracker.PeerAccess
import           Network.Socket
import           Test.Framework                 ( defaultMain
                                                , testGroup
                                                )
import           Test.Framework.Providers.QuickCheck2
                                                ( testProperty )
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Data.Digest.SHA1
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.ByteString               as B

main = defaultMain tests

tests =
  [ testGroup
    "AddGroup"
    [ testProperty "SizeIncreases" prop_size_increases
    , testProperty "SizeRemains"   prop_add_size_same
    ]
  , testGroup
    "RemoveGroup"
    [ testProperty "SizeRemains"   prop_remove_size_same
    , testProperty "SizeDecreases" prop_size_decreases
    ]
  , testGroup "Consistency" [testProperty "SizeSame" prop_sum_sizes]
  , testGroup
    "GetPeers"
    [ testProperty "GetCorrectNumber"     prop_get_correct_peers
    , testProperty "NoDuplicates"         prop_no_duplicate_peers
    , testProperty "ConsistentBeforAfter" prop_get_peers_consistent
    ]
  , testGroup
    "HasPeer"
    [ testProperty "AddPeerHas"            prop_add_peer_has
    , testProperty "RemovePeerDoesn'tHave" prop_remove_peer_nohas
    , testProperty "HasAllPeers"           prop_has_all_peers
    ]
  ]

prop_size_increases :: Peer -> RdmPeerList -> StdGen -> Bool
prop_size_increases p rpl gen = if hasPeer p rpl
  then (peerSize rpl) == (peerSize (evalRand (addPeer p rpl) gen))
  else (1 + (peerSize rpl)) == (peerSize (evalRand (addPeer p rpl) gen))

prop_remove_size_same :: PeerID -> RdmPeerList -> Bool
prop_remove_size_same pid rpl = if hasPID pid rpl
  then ((peerSize rpl) - 1) == (peerSize (removePID pid rpl))
  else (peerSize rpl) == (peerSize (removePID pid rpl))

prop_add_size_same :: RdmPeerList -> Gen Bool
prop_add_size_same rpl = if peerSize rpl == 0
  then return True
  else do
    mp <- choosePeer rpl
    case mp of
      Nothing -> return False
      Just p  -> do
        g <- arbitrary :: Gen StdGen
        return $ peerSize rpl == peerSize (evalRand (addPeer p rpl) g)

prop_size_decreases :: RdmPeerList -> Gen Bool
prop_size_decreases rpl = if peerSize rpl == 0
  then return True
  else do
    mp <- choosePeer rpl
    case mp of
      Nothing -> return False
      Just p  -> do
        g <- arbitrary :: Gen StdGen
        return $ peerSize rpl - 1 == peerSize (removePID (peerID p) rpl)

choosePeer :: RdmPeerList -> Gen (Maybe Peer)
choosePeer rpl = do
  n <- choose (0, peerSize rpl - 1)
  let (k, r) = M.elemAt n $ rplReverse rpl
      curMap = rplCurrent rpl
      nexMap = rplNext rpl
  return $ M.lookup (r, k) curMap `mplus` M.lookup (r, k) nexMap

prop_sum_sizes :: RdmPeerList -> Bool
prop_sum_sizes rpl =
  M.size (rplCurrent rpl) + M.size (rplNext rpl) == peerSize rpl

getSomePeers :: RdmPeerList -> Gen ([Peer], RdmPeerList, Int)
getSomePeers rpl = do
  count <- oneof [choose (0, peerSize rpl), return $ peerSize rpl + 1]
  gen   <- arbitrary :: Gen StdGen
  let (peers, rpl') = evalRand (getUptoNPeer count rpl) gen
  return (peers, rpl', count)

prop_get_correct_peers :: RdmPeerList -> Gen Bool
prop_get_correct_peers rpl = do
  (peers, _, count) <- getSomePeers rpl
  return $ length peers == min count (peerSize rpl)

prop_no_duplicate_peers :: RdmPeerList -> Gen Bool
prop_no_duplicate_peers rpl = do
  (peers, _, count) <- getSomePeers rpl
  return $ length peers == length (nub peers)

prop_get_peers_consistent :: RdmPeerList -> Gen Bool
prop_get_peers_consistent rpl = do
  (_, rpl', count) <- getSomePeers rpl
  return $ getAllPID rpl == getAllPID rpl'

getAllPID rpl =
  let currSet = S.fromList $ map peerID $ M.elems (rplCurrent rpl)
      nextSet = S.fromList $ map peerID $ M.elems (rplNext rpl)
  in  currSet `S.union` nextSet

prop_add_peer_has :: Peer -> RdmPeerList -> Gen Bool
prop_add_peer_has p rpl = do
  gen <- arbitrary :: Gen StdGen
  return $ hasPeer p (evalRand (addPeer p rpl) gen)

prop_remove_peer_nohas :: RdmPeerList -> Gen Bool
prop_remove_peer_nohas rpl = if peerSize rpl == 0
  then return True
  else do
    mp <- choosePeer rpl
    case mp of
      Nothing -> return False
      Just p  -> return $ not $ hasPeer p (removePID (peerID p) rpl)

prop_has_all_peers :: RdmPeerList -> Bool
prop_has_all_peers rpl =
  let currMap = rplCurrent rpl
      nextMap = rplNext rpl
  in  all (`hasPeer` rpl) $ M.elems currMap ++ M.elems nextMap

instance Arbitrary Word160 where
  arbitrary = liftM5 Word160 arbitrary arbitrary arbitrary arbitrary arbitrary
  shrink (Word160 a b c d e) = do
    [a', b', c', d', e'] <- mapM shrink [a, b, c, d, e]
    return $ Word160 a' b' c' d' e'

instance Arbitrary PortNumber where
  arbitrary = liftM (fromIntegral :: Word16 -> PortNumber) arbitrary
  shrink    = return

instance Arbitrary SockAddr where
  arbitrary = oneof
    [ liftM2 SockAddrInet arbitrary arbitrary
    , liftM4 SockAddrInet6 arbitrary arbitrary arbitrary arbitrary
    ]
  shrink (SockAddrInet port addr) = do
    port' <- shrink port
    addr' <- shrink addr
    return $ SockAddrInet port' addr'

  shrink (SockAddrInet6 flow port scope addr) = do
    port'  <- shrink port
    addr'  <- shrink addr
    flow'  <- shrink flow
    scope' <- shrink scope
    return $ SockAddrInet6 flow' port' scope' addr'

instance Arbitrary Peer where
  arbitrary = liftM2 Peer arbitrary arbitrary
  shrink peer = do
    peerid   <- shrink (peerID peer)
    peeraddr <- shrink (peerAddr peer)
    return Peer { peerID = peerid, peerAddr = peeraddr }

instance Arbitrary RdmPeerList where
  arbitrary = do
    count <- frequency
      [(1, return 0), (3, return 10), (3, return 100), (3, return 1000)]
    peers       <- replicateM count arbitrary
    gen         <- arbitrary :: Gen StdGen
    requestSize <- choose (0, count)
    let (rpl, gen') = runRand (foldM (flip addPeer) emptyPL peers) gen
        (_  , rpl') = evalRand (getUptoNPeer requestSize rpl) gen'
    return rpl'

  shrink rpl =
    let rSize = peerSize rpl
    in  if rSize == 0
          then []
          else do
            x <- [0 .. rSize - 1]
            let (k, r) = M.elemAt x (rplReverse rpl)
            return RdmPeerList { rplCurrent = M.delete (r, k) (rplCurrent rpl)
                               , rplNext    = M.delete (r, k) (rplNext rpl)
                               , rplReverse = M.delete k (rplReverse rpl)
                               }

instance Arbitrary StdGen where
  arbitrary = liftM mkStdGen arbitrary
