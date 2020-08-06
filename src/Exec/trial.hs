{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


module Main where
import           Control.Monad.IO.Class
import           Data.Torrent.DB
import           Network.Torrent.Tracker.AnnounceReqTypes
import           Database.MongoDB.Query
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
import           Data.Maybe
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.Text                     as T


data Torrent

instance Accept Torrent where
  contentType _ = B.pack "application" // B.pack "x-bittorrent"

instance MimeRender Torrent BEncode where
  mimeRender _ = bPack

type UploadAPI
  = "upload" :> MultipartForm Mem  (MultipartData Mem) :> Post '[PlainText] String

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
    Just p' -> do
      res <- liftIO $ access p' master db $ addTorrent file
      if res then return "True" else return "False"
    otherwise -> return "False"

mpserver :: Server UploadAPI
mpserver = mphandler

mpproxy :: Proxy UploadAPI
mpproxy = Proxy

app :: Application
app = serve mpproxy mpserver

main :: IO ()
main = run 8080 app
