{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad
import Control.Monad.IO.Class
import DB.Access
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text as T
import Database.MongoDB
import Database.MongoDB.Connection
import Network.HTTP.Media ((//))
import Network.URI.Encode
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.Multipart
import Torrent.Conversions
import Torrent.Types
import Types

type UploadAPI = "upload" :> ReqBody '[JSON] UploadReq :> Post '[JSON] String

type SearchAPI = "search" :> Capture "search" T.Text :> Get '[JSON] [ResultCard]

type DescAPI =
  "torrent" :> "desc" :> Capture "infoHash" String :> Get '[JSON] TorrentDesc

type DlAPI = "torrent" :> "download" :> Capture "infoHash" String :> Get '[TorrentMsg] (Headers '[Header "Content-Disposition" String] Torrent)

type FullAPI = UploadAPI :<|> SearchAPI :<|> DescAPI :<|> DlAPI

data TorrentMsg

instance Accept TorrentMsg where
  contentType _ = "application" // "x-bittorrent"

instance MimeRender TorrentMsg Torrent where
  mimeRender _ torr = torrentToBencode torr

fullProxy :: Proxy FullAPI
fullProxy = Proxy

dlHandler :: Pipe -> String -> Handler (Headers '[Header "Content-Disposition" String] Torrent)
dlHandler pipe ifH = do
  res <- liftIO (dbName >>= \db -> access pipe master db $ dlTorrent ifH)  
  case res of
    (Just torr) -> return $ addHeader "attachment; filename=dl.torrent" torr
    Nothing -> error "Problem downloading Torrent"

uploadHandler :: Pipe -> UploadReq -> Handler String
uploadHandler pipe form = do
  B8.unpack <$> liftIO (dbName >>= \db -> access pipe master db $ uploadTorrent form)

searchHandler :: Pipe -> T.Text -> Handler [ResultCard]
searchHandler pipe query = do
  liftIO
    (dbName >>= \db -> access pipe master db $ searchTorrent $ decodeText query)

descHandler :: Pipe -> String -> Handler TorrentDesc
descHandler pipe ifH = do
  tdesc <- liftIO (dbName >>= \db -> access pipe master db $ getTorrent ifH)
  case tdesc of
    (Just tdesc') -> return tdesc'
    Nothing -> error "problem fetching torrent"

fullServer :: Pipe -> Server FullAPI
fullServer p = uploadHandler p :<|> searchHandler p :<|> descHandler p :<|> dlHandler p

app :: Pipe -> Application
app = corsConfig . serve fullProxy . fullServer

startApp :: Pipe -> IO ()
startApp = run 8080 . app

corsConfig :: Middleware
corsConfig =
  cors
    ( const $
        Just simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}
    )