{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import           Network.Torrent.Tracker.AnnounceReqTypes
import           Network.Torrent.Tracker.AnnounceSrv
import           Network.Torrent.Tracker.AnnounceUDP
import           Servant

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Applicative
import           Control.Monad.IO.Class

import           Data.BEncode
import           Data.Word
import           Data.Either
import           Data.Digest.SHA1
import           Data.Maybe
import           Data.Binary.Get (runGet)
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary.Put (runPut)
import           Data.ByteString.Lazy(fromStrict, toStrict)
import           Data.Text.Encoding (encodeUtf8Builder) 
import           Data.Binary.Builder (toLazyByteString)

import           Network.Wai.Handler.Warp
import           Network.Socket 
import           Network.Socket.ByteString

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Binary as Bin
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AttoT

import qualified Network.HTTP.Types.URI as URI

main :: IO ()
main = do
  emST <- emptyAnnounceST 
  let env = AnnounceEnv {
                       anSt=emST,
                       anConf=defConf   
                        }
  forkIO $ servantThread env
  forkIO $ pruneInactiveThread env
  forkIO $ udpServerThread env 
  forever $ threadDelay oneMinuteMicroS 

parseEvent :: Maybe String -> Maybe Event 
parseEvent = let    cmp = string (B8.pack "completed") *> pure Completed
                    str = string (B8.pack "started") *> pure Started
                    stp = string (B8.pack "stopped") *> pure Stopped
                  in  either (\x -> Nothing) (\x -> Just x) . parseOnly (cmp <|> str <|> stp) . B8.pack . fromMaybe ""

parseWord160 :: (Bin.Binary a) => Maybe String -> a
parseWord160 = Bin.decode . fromStrict . URI.urlDecode False . B8.pack . fromJust  

w32toInteger :: Word32 -> BEncode
w32toInteger x = (BInt . read . show) x

bencodeRes' :: AnnounceRes -> BEncode
bencodeRes' (Failure msg) = BDict $ M.fromList [("failure", (BString $ fromStrict msg))]
bencodeRes' (PeerList itvl (Just seed) (Just leech) peers) =
  BDict $ M.fromList
    [ ("interval"  , (w32toInteger itvl))
    , ("peers"     , (bencodePeers peers))
    , ("complete"  , (w32toInteger seed))
    , ("incomplete", (w32toInteger leech))
    ]
bencodeRes' (PeerList itvl Nothing Nothing peers) =
  BDict $ M.fromList
    [("interval", (w32toInteger itvl)), ("peers", (bencodePeers peers))]


textToW160 :: T.Text -> Word160 
textToW160 = Bin.decode . fromStrict  . URI.urlDecode False . toStrict . toLazyByteString . encodeUtf8Builder  

instance MimeRender PlainText BEncode where
  mimeRender _ = bPack 

type ReqAPI = "announce" 
            :> QueryParam "info_hash" T.Text 
            :> QueryParam "peer_id" T.Text
            :> QueryParam "port" Integer 
            :> QueryParam "uploaded" Word64
            :> QueryParam "downloaded" Word64
            :> QueryParam "left" Word64
            :> QueryFlag "compact" 
            :> QueryFlag "no_peer_id"
            :> QueryParam "event" String
            :> QueryParam "ip" String 
            :> QueryParam "numwant" Word32 
            :> QueryParam "key" String 
            :> RemoteHost 
            :> Servant.Get '[PlainText] BEncode

type ScrapeAPI = "scrape" :> QueryParams "info_hash" T.Text :> Servant.Get '[PlainText] BEncode 
type FullAPI = ReqAPI :<|> ScrapeAPI

fullServe :: AnnounceEnv -> Server FullAPI
fullServe env = (reqHandler env) :<|> (scrapeHandler env) 

fullAPI :: Proxy FullAPI 
fullAPI = Proxy

scrapeAPI :: Proxy ScrapeAPI
scrapeAPI = Proxy

scrapeHandler :: AnnounceEnv -> [T.Text] -> Handler BEncode
scrapeHandler env ihs = do 
    emSt <- liftIO emptyAnnounceST
    let ihs' =  map textToW160 ihs
    let res  = handleScrape IP4 ihs' 
    op <- liftIO $ runReaderT res env
    let ls = zipWith (\ih sr -> (B8.unpack . w160toBString $ ih, sr)) ihs' . map bencodeScrape $ op 
    return . BDict . M.fromList $ ls


reqHandler :: AnnounceEnv  
           -> Maybe T.Text -> Maybe T.Text 
           -> Maybe Integer -> Maybe Word64
           -> Maybe Word64 -> Maybe Word64
           -> Bool        -> Bool
           -> Maybe String -> Maybe String
           -> Maybe Word32 -> Maybe String 
           -> SockAddr -> Handler BEncode
reqHandler env ih pid pn up dl lf cpt npid ev ip nw k clAddr = 
  do
    emST <- liftIO emptyAnnounceST
    let ih'  = textToW160 $ fromMaybe (error "info_hash failed") ih
        pid' = textToW160 $ fromMaybe (error "peer_id failed") pid
        pn'  = fromIntegral $ fromMaybe (error "PortNumber failed") pn
        up'  = fromMaybe 0  up
        dl'  = fromMaybe 0  dl
        lf'  = fromMaybe 0  lf 
        ev'  = parseEvent ev
        cla' = chAddr pn' clAddr
        req  = AnnounceReq {
                  anInfoHash=ih',
                  anPeer=Peer{peerID=pid',peerAddr=cla'},
                  anUploaded=up',
                  anDownloaded=dl',
                  anWant=nw,
                  anEvent=ev',
                  anLeft=lf' 
                }
    liftIO $ print req
    let res  = liftM bencodeRes'.  runReaderT (handleAnnounce req) $  env
    liftIO res 
    where
     chAddr :: PortNumber -> SockAddr -> SockAddr 
     chAddr pn (SockAddrInet _ hr) = SockAddrInet pn hr
     chAddr pn (SockAddrInet6 _ a b c) = SockAddrInet6 pn a b c 

app :: AnnounceEnv -> Application 
app env = serve fullAPI $ fullServe env  

oneMinuteMicroS :: Int
oneMinuteMicroS = 60 * 1000 * 1000

pruneInactiveThread :: AnnounceEnv -> IO ()
pruneInactiveThread anEnv = forever $ do 
  threadDelay oneMinuteMicroS 
  runReaderT pruneQueue anEnv 
  
servantThread :: AnnounceEnv -> IO ()
servantThread env = do
  print "Starting HTTP Tracker"
  Network.Wai.Handler.Warp.run 6969 $ app env 

udpServerThread :: AnnounceEnv -> IO ()
udpServerThread anEnv = do
  print "Started UDP Tracker"
  env <- makeUdpEnv anEnv
  forM_ (ancAddrs $ anConf anEnv) $ \(addr, port) -> do
    putStrLn $ "Binding to " ++ addr ++ ":" ++ port 
    addrinfos <- getAddrInfo Nothing (Just addr) (Just port)
    let serverAddr = head addrinfos
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol 
    when (addrFamily serverAddr == AF_INET6) $ 
      setSocketOption sock IPv6Only 1
    bind sock (addrAddress serverAddr)
    forkIO $ acceptAndProcessRequests sock env
    forkIO $ cycleKeyThread env 
  where
    acceptAndProcessRequests :: Socket -> UdpEnv -> IO ()
    acceptAndProcessRequests sock env = forever $ do 
      (msg, addr) <- recvFrom sock 1024
      forkIO $
        runReaderT (handleUdpRequest sock addr msg) env
    cycleKeyThread env = forever $ do
      threadDelay (2 * oneMinuteMicroS)
      runReaderT cycleKeys env
