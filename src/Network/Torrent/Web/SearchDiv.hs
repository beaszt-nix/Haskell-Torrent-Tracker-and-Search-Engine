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

searchBar :: H.Html
searchBar =
  H.div
    ! A.class_ "header"
    $ H.div
    ! A.class_ "home-menu pure-menu pure-menu-horizontal"
    $ do
        H.a ! A.class_ "pure-menu-heading" ! A.href "/home" $ "P2P File Share"
        H.ul
          ! A.class_ "pure-menu-list"
          $ H.li
          ! A.class_ "pure-menu-item"
          $ H.a
          ! A.href "/upload.html"
          ! A.class_ "pure-menu-link"
          $ "Upload Torrent"
        H.li
          ! A.class_ "pure-menu-item"
          $ H.form
          ! A.method "get"
          ! A.action "/search"
          ! A.class_ "pure-form"
          $ H.fieldset
          $ do
              H.input
                ! A.type_ "text"
                ! A.class_ "pure-input-1"
                ! A.name "query"
                ! A.placeholder "Enter Text"
              H.button
                ! A.type_ "submit"
                ! A.class_ "pure-button pure-button-primary"
                $ "Search Now"

searchBar' :: H.Html
searchBar' =
  H.div
    ! A.class_ "header"
    $ H.div
    ! A.class_ "home-menu pure-menu pure-menu-horizontal pure-menu-fixed"
    $ do
        H.a ! A.class_ "pure-menu-heading" ! A.href "/home" $ "P2P File Share"
        H.ul
          ! A.class_ "pure-menu-list"
          $ H.li
          ! A.class_ "pure-menu-item"
          $ H.a
          ! A.href "/upload.html"
          ! A.class_ "pure-menu-link"
          $ "Upload Torrent"
        H.li
          ! A.class_ "pure-menu-item"
          $ H.form
          ! A.method "get"
          ! A.action "/search"
          ! A.class_ "pure-form"
          $ H.fieldset
          $ do
              H.input
                ! A.type_ "text"
                ! A.class_ "pure-input-1"
                ! A.name "query"
                ! A.placeholder "Enter Text"
              H.button
                ! A.type_ "submit"
                ! A.class_ "pure-button pure-button-primary"
                $ "Search Now"
descr :: Bson.Document -> H.Html
descr doc = H.docTypeHtml ! A.lang "en" $ do
  let (Bson.String title          ) = fromJust $ Bson.look "tname" doc
      (Bson.Doc    info           ) = fromJust $ Bson.look "info" doc
      (Bson.Bin    (Bson.Binary x)) = fromJust $ Bson.look "info_hash" doc
      (Bson.String desc'          ) = fromJust $ Bson.look "comment" doc
      (Bson.String auth) =
        fromMaybe (Bson.String "Anonymous") $ Bson.look "created by" doc
      (Bson.String name ) = fromJust $ Bson.look "name" info
      (Bson.Array  files) = fromJust $ Bson.look "files" info
      files'              = catMaybes $ map f files
      info_hash           = B8.unpack . B16.encode . B8.fromStrict $ x
      downloadlink        = fromString $ "/download?info_hash=" ++ info_hash
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content
      "width=device-width, initial-scale=1.0"
    H.meta ! A.name "description"
    H.title $ H.toHtml title
    H.link ! A.rel "stylesheet" ! A.href
      "https://unpkg.com/purecss@2.0.3/build/pure-min.css"
    H.link ! A.rel "stylesheet" ! A.href "/stylesDesc.css"
    H.link ! A.rel "stylesheet" ! A.href "/stylesLanding.css"
  H.body $ H.div ! A.id "layout" $ do
    searchBar'
    H.br
    H.div ! A.id "main" $ do
      H.div ! A.class_ "header" $ do
        H.h1 $ H.toHtml title
        H.h2 $ H.toHtml auth
      H.div ! A.class_ "content" $ do
        H.h2
          ! A.class_ "content-subhead"
          $ H.a
          ! A.href downloadlink
          $ "Download Here"
        H.h2 ! A.class_ "content-subhead" $ "Description"
        H.p $ H.toHtml desc'
        H.h2 ! A.class_ "content-subhead" $ "File List"
        H.div ! A.id "nav" $ do
          H.ul $ do
            forM_ files' $ \(path, size) -> H.li $ do
              H.toHtml $ T.intercalate "/" [name, path]
              ": "
              H.toHtml size where
  f :: Bson.Value -> Maybe (T.Text, Int32)
  f (Bson.Doc a) = do
    (Bson.String path) <- Bson.look "path" a
    (Bson.Int32  size) <- Bson.look "length" a
    return (path, size)
  f _ = Nothing

landing :: H.Html
landing = H.docTypeHtml ! A.lang "en" $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content
      "width=device-width, initial-scale=1.0"
    H.title "A.P2P File Sharing System"
    H.link ! A.rel "stylesheet" ! A.href
      "https://unpkg.com/purecss@2.0.3/build/pure-min.css"
    H.link
      ! A.rel "stylesheet"
      ! A.href
          "https://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css"
    H.link ! A.rel "stylesheet" ! A.href "/stylesLanding.css"
  H.body $ do
    H.div
      ! A.class_ "header"
      $ H.div
      ! A.class_ "home-menu pure-menu pure-menu-horizontal pure-menu-fixed"
      $ do
          H.a ! A.class_ "pure-menu-heading" ! A.href "/home" $ "P2P File Share"
          H.ul
            ! A.class_ "pure-menu-list"
            $ H.li
            ! A.class_ "pure-menu-item"
            $ H.a
            ! A.href "/upload.html"
            ! A.class_ "pure-menu-link"
            $ "Upload Torrent"
    H.div ! A.class_ "splash-container" $ H.div ! A.class_ "splash" $ do
      H.h1 ! A.class_ "splash-head" $ "P2P File Sharing System"
      H.p ! A.class_ "splash-subhead" $ "Enter Search Term"
      H.form
        ! A.method "get"
        ! A.action "/search"
        ! A.class_ "pure-form"
        $ H.fieldset
        $ do
            H.input
              ! A.type_ "text"
              ! A.class_ "pure-input-1"
              ! A.name "query"
              ! A.placeholder "Enter Text"
            H.button
              ! A.type_ "submit"
              ! A.class_ "pure-button pure-button-primary"
              $ "Search Now"

searchpage :: [SearchRes] -> H.Html
searchpage sr = H.docTypeHtml ! A.lang "en" $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content
      "width=device-width, initial-scale=1.0"
    H.title "P2P File Sharing Results"
    H.link ! A.rel "stylesheet" ! A.href
      "https://unpkg.com/purecss@2.0.3/build/pure-min.css"
    H.link ! A.rel "stylesheet" ! A.href "/stylesLanding.css"
  H.body $ do
    H.div $ searchBar
    H.br
    H.div $ H.section $ H.div $ H.div $ do
      forM_ sr searchDiv

searchDiv :: SearchRes -> H.Html
searchDiv sr = do
  H.div $ do
    let k =
          ( fromString
            . B8.unpack
            . B8.append (B8.pack "/search_result?info_hash=")
            . B16.encode
            . encode
            . srInfoHash
            )
            sr
    H.span $ H.a ! A.href k $ H.toHtml $ srTorrName sr
    H.br
    H.span $ do
      H.span $ H.toHtml $ fromMaybe "Anonymous" $ srCreatedBy sr
    H.br
    H.span $ H.toHtml $ fromMaybe "No Comment" $ srComment sr
  H.br
