{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Main where
import           Control.Monad.IO.Class
import           Data.Torrent.DB
import           Network.Torrent.Tracker.AnnounceReqTypes
import           Control.Monad.Reader           ( runReaderT )
import           Database.MongoDB.Query
import           Database.MongoDB.Connection
import           Servant
import           Servant.API
import           Data.Torrent.Search
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
type SearchResAPI
  = "search_result" :> QueryParam "info_hash" String :> Get '[HTML] H.Html
type UDAPI
  = ("home" :> Get '[HTML] H.Html) :<|> UploadAPI :<|> DownloadAPI :<|> ResultsAPI :<|> SearchResAPI  :<|> Raw


searchresHandler :: Pipe -> Maybe String -> Handler H.Html
searchresHandler p string = do
  let infoHash = Bin.decode . fst . B16L.decode . BL.pack . fromJust $ string
  db <- liftIO $ torrDB
  a  <- liftIO $ access p master db $ getTorrentFromIH' infoHash
  return $ descr a

searchHandler :: SearchEnv -> Pipe -> Maybe T.Text -> Handler H.Html
searchHandler se p t = do
  case t of
    Just t -> do
      db <- liftIO torrDB
      c  <- liftIO $ access p master db $ getTorrentFromText t se
      sr <- liftIO $ access p master db $ restTorrent c
      liftIO $ print sr
      return $ searchpage sr
    Nothing -> return $ searchpage []

dlhandler :: Pipe -> Maybe String -> Handler (Maybe BEncode)
dlhandler p string = do
  let infoHash = Bin.decode . fst . B16L.decode . BL.pack . fromJust $ string
  db <- liftIO $ torrDB
  x  <- liftIO $ access p master db $ getTorrentFromIH infoHash
  return x

mpserver :: SearchEnv -> Pipe -> Server UploadAPI
mpserver se p = mphandler se p
 where
  mem :: Proxy Mem
  mem = Proxy

  mphandler :: SearchEnv -> Pipe -> MultipartData Mem -> Handler String
  mphandler se p mpdata = do
    let (Right a) = lookupFile (T.pack "torrent") mpdata
        descr     = lookupInput (T.pack "description") mpdata
    let file = fdPayload a :: BL.ByteString
        name = T.reverse . T.drop 8 . T.reverse . fdFileName $ a
    liftIO $ print descr
    db <- liftIO $ torrDB
    liftIO $ access p master db $ addTorrent name file se

fullserver :: SearchEnv -> Pipe -> Server UDAPI
fullserver se p =
  homeHandler
    :<|> (mpserver se p)
    :<|> (dlhandler p)
    :<|> searchHandler se p
    :<|> searchresHandler p
    :<|> serveDirectoryWebApp "/var/www/html"
  where homeHandler = return landing

fullproxy :: Proxy UDAPI
fullproxy = Proxy

app :: SearchEnv -> Pipe -> Application
app p se = serve fullproxy $ fullserver p se

main :: IO ()
main = do
  p <- liftIO $ connToDB
  case p of
    Just p' -> emptySearchEnv >>= \x -> run 8080 $ app x p'
    Nothing -> error "Unable to connect to database"
