module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.Monoid
import           Network.Torrent.Tracker.AnnounceReqTypes
import           Network.Torrent.Tracker.AnnounceServer
import           Network.Torrent.Tracker.AnnounceSrv
import           Network.Socket          hiding ( send
                                                , sendTo
                                                , recv
                                                , recvFrom
                                                )
import           Network.Socket.ByteString
import           Snap.Http.Server        hiding ( defaultConfig )
import           Snap.Http.Server.Config hiding ( defaultConfig )
import qualified Data.ByteString.Char8         as B8

oneMinuteMicros :: Int
oneMinuteMicros = 60 * 1000 * 1000

-- | Thread to prune inactive hashes from the maps
pruneInactiveThread :: AnnounceEnv -> IO ()
pruneInactiveThread anEnv = forever $ do
  threadDelay oneMinuteMicros
  runReaderT pruneQ anEnv

-- | Http server thread
snapServerThread :: AnnounceEnv -> IO ()
snapServerThread env = do
  putStrLn "Starting snap server."
  let baseConfig = setVerbose True mempty
  forM_ (ancAddrs $ anConf env) $ \(addr, port) -> do
    let config =
          setAccessLog (ConfigFileLog ("log/access." ++ addr ++ ".log"))
            $ setErrorLog (ConfigFileLog ("log/error." ++ addr ++ ".log"))
            $ setBind (B8.pack addr)
            $ setPort (read port) baseConfig
    forkIO $ httpServe config (torrentSnap env)

main = do
  anSt <- emptyAnnounceST
  let anEnv = AnnounceEnv { anSt = anSt, anConf = defConf }
  forkIO $ pruneInactiveThread anEnv
  forkIO $ snapServerThread anEnv
  forever $ threadDelay oneMinuteMicros
