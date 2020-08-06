{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module Main where
import           Control.Monad.IO.Class
import           Data.Torrent.DB
import           Network.Torrent.Tracker.AnnounceReqTypes
import           Database.MongoDB.Query
import           Database.MongoDB.Connection
import           Servant
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.Multipart
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Data.BEncode
import           Data.Either
import qualified Data.Binary                   as Bin
import           Data.Maybe
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.ByteString.Base16.Lazy   as B16L
import qualified Data.Text                     as T


data Torrent

instance Accept Torrent where
  contentType _ = B.pack "application" // B.pack "x-bittorrent"

instance MimeRender Torrent BEncode where
  mimeRender _ = bPack

type UploadAPI
  = "upload" :> MultipartForm Mem  (MultipartData Mem) :> Post '[PlainText] String

type DownloadAPI = "download" :> 
                    QueryParam "info_hash" String :>
                    Get '[Torrent] BEncode

type UDAPI = UploadAPI :<|> DownloadAPI :<|> Raw

dlhandler :: Maybe String -> Handler BEncode 
dlhandler string = do
  let infoHash = Bin.decode . fst . B16L.decode . BL.pack . fromJust $ string 
  pipe        <- liftIO $ connToDB
  case pipe of 
    (Just p) -> do 
            db       <- liftIO $ torrDB  
            x        <- liftIO $ access p master db $ getTorrentFromIH [infoHash]
            rs       <- liftIO $ access p master db $ restTorrent x 
            liftIO $ close p 
            return $ head rs
    otherwise -> return $ error "Pipe couldn't be made"

fullserver :: Server UDAPI 
fullserver = mphandler :<|> dlhandler :<|> serveDirectoryWebApp "/var/www"

fullproxy :: Proxy UDAPI
fullproxy = Proxy

mem :: Proxy Mem
mem = Proxy

mphandler :: MultipartData Mem -> Handler String
mphandler mpdata = do
  let (Right a)   = lookupFile (T.pack "torrent") mpdata
      description = either (\_ -> "No Description") (T.unpack)
        $ lookupInput (T.pack "description") mpdata
  p <- liftIO $ connToDB
  let file = fdPayload a :: BL.ByteString
  db <- liftIO $ torrDB
  case p of
    Just p' -> 
      liftIO $ access p' master db $ addTorrent (fdFileName a) (T.pack description) file
    otherwise -> return "False"

mpserver :: Server UploadAPI
mpserver = mphandler

mpproxy :: Proxy UploadAPI
mpproxy = Proxy

app :: Application
app = serve fullproxy fullserver 

main :: IO ()
main = run 8080 app
