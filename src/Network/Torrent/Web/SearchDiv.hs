{-# LANGUAGE OverloadedStrings #-}

module Network.Torrent.Web.SearchDiv where

import           Text.Blaze.Html
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import           Data.String                    ( fromString )
import           Control.Monad                  ( forM_ )
import qualified Text.Blaze.Html5.Attributes   as A
import           Data.Torrent.DB
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
  H.div ! A.class_ "pure-u-1" ! A.style "border-style: dotted" $ do
    H.p $ H.toHtml $ srDesc sr
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
