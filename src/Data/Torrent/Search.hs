{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Torrent.Search where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Char
import           Data.Maybe
import qualified Data.Bson                     as Bson
import qualified Data.Text                     as T
import qualified Data.HashSet                  as HS
import qualified Data.Text.IO                  as TIO

newtype SearchEnv = SearchEnv { stopword :: MVar (HS.HashSet T.Text) }
type SearchT = ReaderT SearchEnv IO

filterSpecial :: Char -> T.Text
filterSpecial c = if isAlphaNum c then T.singleton c else T.singleton ' '

textproc :: T.Text -> T.Text
textproc = T.concatMap (T.toLower . filterSpecial)

getNGrams :: Int -> T.Text -> [T.Text]
getNGrams n text
  | (T.length text) > n  = (T.take n text) : (getNGrams n $ T.tail text)
  | (T.length text) <= n = T.take n text : []

loadStopWords :: IO (HS.HashSet T.Text)
loadStopWords = do
  res <- TIO.readFile "/home/anjan/lang-prac/stop"
  return $ HS.fromList $ T.lines res

emptySearchEnv :: IO SearchEnv
emptySearchEnv = do
  stopM <- newMVar =<< loadStopWords
  return $ SearchEnv { stopword = stopM }

genDocNgram :: Bson.Document -> SearchT Bson.Value
genDocNgram doc = do
  (Bson.String comment) <- Bson.look "comment" doc
  (Bson.String tname  ) <- Bson.look "tname" doc
  (Bson.String creator) <- Bson.look "created by" doc
  (Bson.Doc    info   ) <- Bson.look "info" doc
  (Bson.String name   ) <- Bson.look "name" info
  stopWords             <- asks stopword >>= liftIO . readMVar

  let k   = concatMap (T.words . textproc) [comment, tname, creator, name]
      fl3 = concatMap (getNGrams 3) . filter (func stopWords) $ k
      fl4 = concatMap (getNGrams 4) . filter (func stopWords) $ k

  return $ Bson.String $ T.unwords $ concat [fl3, fl4]

 where
  func :: HS.HashSet T.Text -> T.Text -> Bool
  func hs t = not $ HS.member t hs

mkQuery :: T.Text -> SearchT Bson.Value
mkQuery query = do
  let ngrams3 = concatMap (getNGrams 3) $ T.words $ textproc query
      ngrams4 = concatMap (getNGrams 4) $ T.words $ textproc query
  return $ Bson.String $ T.unwords $ concat [ngrams3, ngrams4]
