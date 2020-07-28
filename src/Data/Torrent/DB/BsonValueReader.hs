{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module Data.Torrent.DB.BsonValueReader
  ( BsonValueReader(..)
  , runBsonValueReader
  , bsonstring
  , bsonbinary
  , bsonint
  , bsontime
  , fromOptional
  )
where

import           Control.Applicative
import           Control.Monad                  ( MonadPlus )
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Maybe
import           Data.Int
import           Data.Time.Clock.Compat         ( nominalDiffTimeToSeconds )
import           Data.Time.Clock.POSIX          ( utcTimeToPOSIXSeconds )
import           Data.Maybe                     ( fromMaybe )
import           Data.Bson                      ( cast' )

import qualified Data.Bson                     as Bson
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.Map                      as M
import qualified Data.Text                     as T

newtype BsonValueReader a = BsonValueReader (MaybeT (Reader Bson.Value) a)
           deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

bsonreader :: (Bson.Value -> Maybe a) -> BsonValueReader a
bsonreader = BsonValueReader . MaybeT . reader

runBsonValueReader :: BsonValueReader a -> Bson.Value -> Maybe a
runBsonValueReader (BsonValueReader bs) = runReader $ runMaybeT bs

bsonstring :: BsonValueReader String
bsonstring = bsonreader $ \b -> case b of
  (Bson.String x) -> return $ T.unpack $ fromMaybe T.empty $ cast' b
  _               -> Nothing

bsonbinary :: BsonValueReader L.ByteString
bsonbinary = bsonreader $ \b -> case b of
  Bson.Bin (Bson.Binary x) -> return $ L.fromStrict x
  _                        -> Nothing

bsonint :: BsonValueReader Integer
bsonint = bsonreader $ \b -> case b of
  Bson.Int32 x -> return $ fromIntegral x
  _            -> Nothing

bsontime :: BsonValueReader Integer
bsontime = bsonreader $ \b -> case b of
  Bson.UTC x ->
    (return . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) x
  _ -> Nothing

fromOptional
  :: String
  -> (Bson.Value -> Maybe a)
  -> Maybe Bson.Value
  -> Maybe [(String, a)]
fromOptional _ _ Nothing  = Just []
fromOptional k f (Just x) = case f x of
  Just res -> Just [(k, res)]
  Nothing  -> Just []
