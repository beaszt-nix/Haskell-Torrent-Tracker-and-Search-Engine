{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}

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
import           Servant.HTML.Blaze
import           Network.Torrent.Web.SearchDiv
import           Network.Wai.Handler.Warp       ( run )
import           Data.BEncode
import           Data.Either
import qualified Data.Binary                   as Bin
import           Data.Maybe
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.ByteString.Base16.Lazy   as B16L
import qualified Data.Text                     as T
import qualified Text.Blaze.Html5              as H

data Torrent

instance Accept Torrent where
  contentType _ = B.pack "application" // B.pack "x-bittorrent"

instance MimeRender Torrent (Maybe BEncode) where
  mimeRender _ (Just s) = bPack s
  mimeRender _ Nothing  = BL.pack "Failure. Couldn't find Torrent"

type UploadAPI
  = "upload" :> MultipartForm Mem  (MultipartData Mem) :> Post '[PlainText] String
type DownloadAPI
  = "download" :> QueryParam "info_hash" String :> Get '[Torrent] (Maybe BEncode)
type ResultsAPI = "search" :> QueryParam "query" T.Text :> Get '[HTML] H.Html
type UDAPI = UploadAPI :<|> DownloadAPI :<|> ResultsAPI :<|> Raw


searchHandler :: Pipe -> Maybe T.Text -> Handler H.Html
searchHandler p t = do
  case t of
    Just t -> do
      db <- liftIO torrDB
      c  <- liftIO $ access p master db $ getTorrentFromText t
      sr <- liftIO $ access p master db $ restTorrent c
      return $ searchpage sr
    Nothing -> return $ searchpage []

dlhandler :: Pipe -> Maybe String -> Handler (Maybe BEncode)
dlhandler p string = do
  let infoHash = Bin.decode . fst . B16L.decode . BL.pack . fromJust $ string
  db <- liftIO $ torrDB
  x  <- liftIO $ access p master db $ getTorrentFromIH infoHash
  return x

mpserver :: Pipe -> Server UploadAPI
mpserver p = mphandler p
 where
  mem :: Proxy Mem
  mem = Proxy

  mphandler :: Pipe -> MultipartData Mem -> Handler String
  mphandler p mpdata = do
    let (Right a)   = lookupFile (T.pack "torrent") mpdata
        description = either (\_ -> "No Description") (T.unpack)
          $ lookupInput (T.pack "description") mpdata
    let file = fdPayload a :: BL.ByteString
    db <- liftIO $ torrDB
    liftIO $ access p master db $ addTorrent (fdFileName a)
                                             (T.pack description)
                                             file

fullserver :: Pipe -> Server UDAPI
fullserver p =
  (mpserver p) :<|> (dlhandler p) :<|> searchHandler p :<|> serveDirectoryWebApp
    "/var/www"

fullproxy :: Proxy UDAPI
fullproxy = Proxy

app :: Pipe -> Application
app p = serve fullproxy $ fullserver p

main :: IO ()
main = do
  p <- liftIO $ connToDB
  case p of
    Just p' -> run 8080 $ app p'
    Nothing -> error "Unable to connect to database"
