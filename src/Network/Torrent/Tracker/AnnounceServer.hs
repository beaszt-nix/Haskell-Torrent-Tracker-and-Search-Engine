{-# LANGUAGE RankNTypes #-}

module Network.Torrent.Tracker.AnnounceServer
  ( torrentSnap
  )
where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader


import           Data.Binary.Get
import           Data.Binary
import           Data.Bits
import           Data.ByteString.Lazy.Builder
import           Data.Either
import           Data.Endian
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           Debug.Trace

import           Network.Torrent.Tracker.AnnounceReqTypes
import           Network.Torrent.Tracker.AnnounceSrv
import           Control.Monad.Trans.ContEither
import           Network.Torrent.Tracker.QueryParsers

import           Network.Socket
import           Snap.Core
import           System.Endian

import           Data.ByteString.Lazy           ( toStrict
                                                , fromStrict
                                                )

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Char8         as B8
import qualified Data.Map                      as M

rqAnnounce :: Request -> ContEitherT m B.ByteString AnnounceReq
rqAnnounce req = do
  let params = rqQueryParams req
  hash       <- getValidM (B8.pack "info_hash") params valid20Bytes
  pid        <- getValidM (B8.pack "peer_id") params valid20Bytes
  port       <- getValidM (B8.pack "port") params validInt
  uploaded   <- getValidM (B8.pack "uploaded") params validInt
  downloaded <- getValidM (B8.pack "downloaded") params validInt
  left       <- getValidM (B8.pack "left") params validInt
  addr       <- getValidO (B8.pack "ip") params (validSockAddr port)
  reqAddr    <- validSockAddr port B8.empty (rqClientAddr req)
  compact    <-
    getValidO (B8.pack "compact") params validInt :: ContEitherT
      m
      B.ByteString
      (Maybe Word8)
  failIf (B8.pack "Compact not supported") (isJust compact && compact /= Just 1)
  event <- getValidO (B8.pack "event") params validEvent
  want  <- getValidO (B8.pack "numwant") params validInt
  let realAddr = case reqAddr of
        SockAddrInet6{}      -> reqAddr
        SockAddrInet _ addr4 -> if isRFC1918 addr4
          then case addr of
            Nothing                 -> reqAddr
            Just SockAddrInet6{}    -> reqAddr
            Just a@(SockAddrInet{}) -> a
          else reqAddr
  return AnnounceReq { anInfoHash   = hash
                     , anPeer       = Peer { peerID = pid, peerAddr = realAddr }
                     , anUploaded   = uploaded
                     , anDownloaded = downloaded
                     , anLeft       = left
                     , anEvent      = event
                     , anWant       = want
                     }

announceAction :: AnnounceEnv -> Snap ()
announceAction env = do
  kAnnounce <- getsRequest rqAnnounce
  runContEitherT kAnnounce failure success
 where
  failure message = do
    writeBS message
    getResponse >>= finishWith
  success announce = do
    res <- liftIO $ runReaderT (hdlAnnounce announce) env
    writeLBS $ fromStrict $ encoder res
    getResponse >>= finishWith
   where
    encoder = case peerAddr $ anPeer announce of
      SockAddrInet{}  -> bencodeRes
      SockAddrInet6{} -> undefined

scrapeAction :: AnnounceEnv -> Snap ()
scrapeAction env = do
  mHashList <- getsRequest (rqQueryParam (B8.pack "info_hash"))
  case mHashList of
    Nothing -> do
      writeBS (B8.pack "hashes required")
      getResponse >>= finishWith
    Just val -> do
      let kvals = mapM parse val
          parse = valid20Bytes (B8.pack "info_hash")
      runContEitherT kvals failure success

 where
  success hash = do
    kipVer <- getsRequest rGetIpVer
    let ipVer = runContEither kipVer (const IP4) id
    res <- liftIO $ runReaderT (handleScrape ipVer hash) env
    writeLBS $ fromStrict $ bencodeScrapes $ zip hash res
    getResponse >>= finishWith
  failure msg = do
    writeBS msg
    getResponse >>= finishWith

rGetIpVer :: Request -> ContEitherT m B.ByteString IPVer
rGetIpVer req = do
  let port = (fromIntegral . rqClientPort) req
  reqAddr <- validSockAddr port B8.empty (rqClientAddr req)
  case reqAddr of
    SockAddrInet{}  -> return IP4
    SockAddrInet6{} -> return IP6

torrentSnap :: AnnounceEnv -> Snap ()
torrentSnap env =
  path (B8.pack "announce") (method GET $ announceAction env)
    <|> path (B8.pack "scrape") (method GET $ scrapeAction env)
