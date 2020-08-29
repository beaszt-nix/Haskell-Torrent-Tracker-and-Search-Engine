{-# LANGUAGE OverloadedStrings #-}

module Network.Torrent.Web.SearchDiv where
import           Control.Monad.IO.Class
import           Text.Blaze.Html
import qualified Text.Blaze.Html5              as H
import           Data.Torrent.DB                ( smallSize )
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
          ! A.href "/uploadTut"
          ! A.class_ "pure-menu-link"
          $ "Upload Torrent"
        H.li
          ! A.class_ "pure-menu-item"
          $ H.form
          ! A.method "get"
          ! A.action "/search"
          ! A.class_ "searchBox pure-form"
          $ H.fieldset
          $ do
              H.input
                ! A.type_ "text"
                ! A.class_ "pure-input-1"
                ! A.name "query"
                ! A.placeholder "Enter Text"
              H.input
                ! A.type_ "submit"
                ! A.class_ "pure-button pure-button-primary"
                ! A.value "Search"

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
          ! A.href "/uploadTut"
          ! A.class_ "pure-menu-link"
          $ "Upload Torrent"
        H.li
          ! A.class_ "pure-menu-item"
          $ H.form
          ! A.method "get"
          ! A.action "/search"
          ! A.class_ "searchBox pure-form"
          $ H.fieldset
          $ do
              H.input
                ! A.type_ "text"
                ! A.class_ "pure-input-1"
                ! A.name "query"
                ! A.placeholder "Enter Text"
              H.input
                ! A.type_ "submit"
                ! A.class_ "pure-button pure-button-primary"
                ! A.value "Search"

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
    H.link ! A.rel "stylesheet" ! A.href "/styles/stylesDesc.css"
    H.link ! A.rel "stylesheet" ! A.href "/styles/stylesLanding.css"
  H.body $ H.div ! A.id "layout" $ do
    searchBar'
    H.br >> H.br
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
  f :: Bson.Value -> Maybe (T.Text, T.Text)
  f (Bson.Doc a) = do
    (Bson.String path) <- Bson.look "path" a
    (Bson.Int32  size) <- Bson.look "length" a
    return (path, smallSize . fromIntegral $ size)
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
    H.link ! A.rel "stylesheet" ! A.href "/styles/stylesLanding.css"
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
            ! A.href "/uploadTut"
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
    H.link ! A.rel "stylesheet" ! A.href "/styles/stylesLanding.css"
  H.body $ do
    H.div $ searchBar
    H.br
    H.div $ H.section $ H.div $ H.div ! A.class_ "searchResult" $ do
      forM_ sr searchDiv

searchDiv :: SearchRes -> H.Html
searchDiv sr = do
  H.div ! A.class_ "card" $ do
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
    H.span $ H.toHtml $ srSize sr
    H.br
    H.span $ H.toHtml $ fromMaybe "No Comment" $ srComment sr
  H.br


upload :: H.Html
upload = H.docTypeHtml ! A.lang "en" $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content
      "width=device-width, initial-scale=1.0"
    H.meta
      ! A.name "description"
      ! A.content
          "A layout example with a side menu that hides on mobile, just like the Pure website."
    H.title "P2PFS: Upload Torrent"
    H.link ! A.rel "stylesheet" ! A.href
      "https://unpkg.com/purecss@2.0.3/build/pure-min.css"
    H.link ! A.rel "stylesheet" ! A.href "/styles/stylesUpload.css"
  H.body $ do
    H.div ! A.id "layout" $ do
        --  Menu toggle 
      H.a ! A.href "#menu" ! A.id "menuLink" ! A.class_ "menu-link" $ do
          --  Hamburger icon 
        H.span mempty
      H.div ! A.id "menu" $ H.div ! A.class_ "pure-menu" $ do
        H.span ! A.class_ "pure-menu-heading" $ "P2P File Sharing"
        H.ul
          ! A.class_ "pure-menu-list"
          $ H.li
          ! A.class_ "pure-menu-item"
          $ H.a
          ! A.href "/home"
          ! A.class_ "pure-menu-link"
          $ "Home"
      H.div ! A.id "main" $ do
        H.div ! A.class_ "header" $ do
          H.h1 "How to Upload a Torrent"
          H.h2 "A step by step guide"
        H.div ! A.class_ "content" $ do
          H.h2
            ! A.class_ "content-subhead"
            $ "Know how to create a Torrent already?"
          H.p $ do
            "If you have already been to this page, and know how to perform the following steps, click this link to directly upload: "
            H.a ! A.href "#upload" $ "Upload Now"
          H.h2 ! A.class_ "content-subhead" $ "Get a Torrent Client"
          H.p $ do
            "In order to create a torrent, the first pre-requisite is to have any torrent-client downloaded. We recommend"
            H.a
              ! A.href "https://www.bittorrent.com/downloads/complete/classic/"
              $ "BitTorrent Classic"
            ", As this is what will be used in this guide.\n              If the you have another Torrent client that you prefer, the guide is still valid, as torrent clients are quite similar."
          H.h2 ! A.class_ "content-subhead" $ "Creating the Torrent"
          H.p
            "Open the up the torrent client, and click on the \"Create New Torrent...\" option. \n                Alternately, use the shortcut Ctrl+N (This is a common shortcut across many torrent clients)"
          H.div
            ! A.class_ "pure-g"
            $ H.div
            ! A.class_ "pure-u-1-1"
            $ H.img
            ! A.class_ "pure-img-responsive"
            ! A.src "/screenshots/img1.png"
            ! A.alt "img1.png"
          H.p "Once this is done, the following window pops up"
          H.div
            ! A.class_ "pure-g"
            $ H.div
            ! A.class_ "pure-u-1-1"
            $ H.img
            ! A.class_ "pure-img-responsive"
            ! A.src "/screenshots/img2.png"
            ! A.alt "img2.png"
          H.p
            "In this window, select the file or folder that is to be shared. \n            This part is fairly obvious. \n            In order to utilize the P2P File Sharing System, in the Trackers \n            paste the following"
          H.br
          H.code "http://192.168.29.41:6969/announce"
          H.br
          H.code "udp://192.168.29.41:6969/announce"
          H.p
            "Make sure, to describe the contents of the data being shared using the Comments field. Keep it concise and make sure to put in keywords\n            relevant to the data being shared, and that the checkbox \"Start Seeding\" is ticked. Click Create."
          H.p
            "Here another Window pops up, which allows you to save the .torrent file to any directory, with some name.\n               The name of the .torrent file set here, is going to become the title for its listing in the P2P file sharing system,\n               therefore, ensure that the name is relevant to the data and contains clear keywords. This makes it easier for \n               another user to find this torrent."
          H.div
            ! A.class_ "pure-g"
            $ H.div
            ! A.class_ "pure-u-1-1"
            $ H.img
            ! A.class_ "pure-img-responsive"
            ! A.src "/screenshots/img3.png"
            ! A.alt "img3.png"
        H.div ! A.class_ "content" ! A.id "upload" $ do
          H.h2 ! A.class_ "content-subhead" $ "Upload The Torrent"
          H.form
            ! A.class_ "pure-form pure-form-stacked"
            ! A.method "post"
            ! A.action "/upload"
            ! A.enctype "multipart/form-data"
            $ H.fieldset
            $ do
                H.div ! A.class_ "pure-control-group" $ do
                  H.label ! A.for "torrent" $ "Select File"
                  H.input ! A.type_ "file" ! A.id "torrent" ! A.name "torrent"
                H.div
                  ! A.class_ "pure-control-group"
                  $ H.input
                  ! A.type_ "submit"
                  ! A.class_ "pure-button pure-button-primary"
                  ! A.value "Upload"

    H.script
      "(function (window, document) {\n\n    var layout   = document.getElementById('layout'),\n        menu     = document.getElementById('menu'),\n        menuLink = document.getElementById('menuLink');\n\n    function toggleClass(element, className) {\n        var classes = element.className.split(/\\s+/),\n            length = classes.length,\n            i = 0;\n\n        for (; i < length; i++) {\n            if (classes[i] === className) {\n                classes.splice(i, 1);\n                break;\n            }\n        }\n        // The className is not found\n        if (length === classes.length) {\n            classes.push(className);\n        }\n\n        element.className = classes.join(' ');\n    }\n\n    function toggleAll(e) {\n        var active = 'active';\n\n        e.preventDefault();\n        toggleClass(layout, active);\n        toggleClass(menu, active);\n        toggleClass(menuLink, active);\n    }\n    \n    function handleEvent(e) {\n        if (e.target.id === menuLink.id) {\n            return toggleAll(e);\n        }\n        \n        if (menu.className.indexOf('active') !== -1) {\n            return toggleAll(e);\n        }\n    }\n    \n    document.addEventListener('click', handleEvent);\n\n}(this, this.document));"

