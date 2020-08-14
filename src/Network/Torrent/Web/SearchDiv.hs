{-# LANGUAGE OverloadedStrings #-}

module Network.Torrent.Web.SearchDiv where
import           Control.Monad.IO.Class
import           Text.Blaze.Html
import qualified Text.Blaze.Html5              as H
import           Data.Maybe
import           Data.Int
import           Text.Blaze.Html5               ( (!) )
import           Data.String                    ( fromString )
import           Control.Monad                  ( forM_ )
import qualified Text.Blaze.Html5.Attributes   as A
import           Data.Torrent.DB
import qualified Data.Text                     as T
import qualified Data.Bson                     as Bson
import           Network.Torrent.Tracker.AnnounceReqTypes
import qualified Data.ByteString.Lazy.Char8    as B8
import qualified Data.ByteString.Base16.Lazy   as B16
import           Data.Binary

searchDiv :: SearchRes -> H.Html
searchDiv sr = H.div ! A.class_ "pure-u-1" ! A.style "border-style: solid" $ do
  H.div ! A.class_ "pure-u-1-1" $ do
    let k =
          ( fromString
            . B8.unpack
            . B8.append (B8.pack "/search_result?info_hash=")
            . B16.encode
            . encode
            . srInfoHash
            )
            sr
    H.a ! A.href k $ H.span $ H.toHtml $ srTorrName sr
  H.br
  let x = case srComment sr of
        (Just x) -> x
        Nothing  -> "Anonymous"
  H.br
  H.div ! A.class_ "pure-u-1" ! A.style "border-style: double" $ do
    H.p $ H.toHtml x
  H.br

searchBar :: H.Html
searchBar = H.div $ do
  H.form ! A.class_ "pure-form" ! A.method "get" ! A.action "/search" $ do
    H.fieldset $ do
      H.legend "Search for more torrents here"
      H.input
        ! A.type_ "text"
        ! A.class_ "pure-input"
        ! A.name "query"
        ! A.placeholder "Search here"
      H.button
        ! A.type_ "submit"
        ! A.class_ "pure-button pure-button-primary"
        $ "Find Torrent"
  H.br

searchpage :: [SearchRes] -> H.Html
searchpage srs = H.docTypeHtml $ do
  H.head $ do
    H.link ! A.rel "stylesheet" ! A.href
      "https://unpkg.com/purecss@2.0.3/build/grids-responsive-min.css"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.body $ do
    searchBar
    H.div ! A.class_ "pure-grid" $ do
      forM_ srs (\x -> searchDiv x >> H.br)

descr :: Bson.Document -> H.Html
descr doc = H.docTypeHtml $ do
  let (Bson.String title          ) = fromJust $ Bson.look "tname" doc
      (Bson.Doc    info           ) = fromJust $ Bson.look "info" doc
      (Bson.Bin    (Bson.Binary x)) = fromJust $ Bson.look "info_hash" doc
      (Bson.String desc           ) = fromJust $ Bson.look "comment" doc
      (Bson.String auth) =
        fromMaybe (Bson.String "Anonymous") $ Bson.look "created by" doc
      (Bson.String name ) = fromJust $ Bson.look "name" info
      (Bson.Array  files) = fromJust $ Bson.look "files" info
      files'              = catMaybes $ map f files
      info_hash           = B8.unpack . B16.encode . B8.fromStrict $ x
      downloadlink        = fromString $ "/download?info_hash=" ++ info_hash
  H.head $ do
    H.link ! A.rel "stylesheet" ! A.href
      "https://unpkg.com/purecss@2.0.3/build/grids-responsive-min.css"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.title $ H.toHtml title
  H.body $ do
    searchBar
    H.h1 $ H.toHtml title
    H.div ! A.class_ "pure-grid" $ do
      H.div $ do
        H.h1 $ H.toHtml $ T.append (T.pack "Created By: ") auth
      H.br
      H.div $ do
        "Download at:"
        H.a ! A.href downloadlink $ "Download"
      H.br
      H.h4 "Description"
      H.div $ do
        H.p $ H.toHtml desc
      H.div $ do
        H.h4 "File List"
        H.ul $ do
          forM_ files' $ \(path, size) -> H.li $ do
            H.toHtml $ T.intercalate "/" [name, path]
            ": "
            H.toHtml size
 where
  f :: Bson.Value -> Maybe (T.Text, Int32)
  f (Bson.Doc a) = do
    (Bson.String path) <- Bson.look "path" a
    (Bson.Int32  size) <- Bson.look "length" a
    return (path, size)
  f _ = Nothing
