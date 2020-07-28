module Network.Torrent.Tracker.QueryParsers
  ( validInt
  , validEvent
  , validSockAddr
  , valid20Bytes
  , getValidM
  , getValidO
  , failIf
  )
where

import           Network.Socket
import           Control.Monad.Trans.ContEither
import           Network.Torrent.Tracker.AnnounceReqTypes

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import           Data.Word
import           Data.Endian
import           System.Endian

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Char8         as B8
import qualified Data.Map                      as M

mapLeft :: (l1 -> l2) -> ContEitherT m l1 a -> ContEitherT m l2 a
mapLeft f ka = ContEitherT $ \lk rk -> runContEitherT ka (lk . f) rk

errorMsg :: B.ByteString -> B.ByteString
errorMsg k = k <> B8.pack " not formatted correctly"

parseOnlyMsg :: b -> B.ByteString -> Parser a -> ContEitherT m b a
parseOnlyMsg msg str p = mapLeft (const msg) $ liftE $ parseOnly p str

maybeParse
  :: B.ByteString -> B.ByteString -> Parser a -> ContEitherT m B.ByteString a
maybeParse name = parseOnlyMsg (errorMsg name)

validInt
  :: Integral a => B.ByteString -> B.ByteString -> ContEitherT m B.ByteString a
validInt n val = maybeParse n val decimal

validEvent :: B.ByteString -> B.ByteString -> ContEitherT m B.ByteString Event
validEvent key stg =
  let cmp = string (B8.pack "completed") *> pure Completed
      str = string (B8.pack "started") *> pure Started
      stp = string (B8.pack "stopped") *> pure Stopped
  in  maybeParse key stg (cmp <|> str <|> stp)

parseIPv4 :: Parser HostAddress
parseIPv4 =
  (   packBytes
  <$> decimal
  <*  char '.'
  <*> decimal
  <*  char '.'
  <*> decimal
  <*  char '.'
  <*> decimal
  )
 where
  packBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
  packBytes b1 b2 b3 b4 =
    fromIntegral b1
      `shiftL` 24
      .|.      fromIntegral b2
      `shiftL` 16
      .|.      fromIntegral b3
      `shiftL` 8
      .|.      fromIntegral b4

parseIPv6 :: Parser HostAddress6
parseIPv6 = nonCompact
 where
  packHex6
    :: Word16
    -> Word16
    -> Word16
    -> Word16
    -> Word16
    -> Word16
    -> Word16
    -> Word16
    -> HostAddress6
  packHex6 a b c d e f g h =
    (byteflip a b, byteflip c d, byteflip e f, byteflip g h)

  byteflip :: Word16 -> Word16 -> Word32
  byteflip a b = toBE32 $ fromIntegral a `shiftL` 16 .|. fromIntegral b

  nonCompact =
    (   packHex6
    <$> hexadecimal
    <*  char ':'
    <*> hexadecimal
    <*  char ':'
    <*> hexadecimal
    <*  char ':'
    <*> hexadecimal
    <*  char ':'
    <*> hexadecimal
    <*  char ':'
    <*> hexadecimal
    <*  char ':'
    <*> hexadecimal
    <*  char ':'
    <*> hexadecimal
    )

validSockAddr
  :: PortNumber
  -> B.ByteString
  -> B.ByteString
  -> ContEitherT m B.ByteString SockAddr
validSockAddr pnum name str =
  let ipParse =
          (SockAddrInet pnum <$> parseIPv4)
            <|> (SockAddrInet6 pnum 0 <$> parseIPv6 <*> pure 0)
  in  maybeParse name str ipParse

valid20Bytes
  :: B.ByteString -> B.ByteString -> ContEitherT m B.ByteString B.ByteString
valid20Bytes name str = case B.length str of
  20 -> case runGetOrFail get (BL.fromStrict str) of
    Left  _                    -> left (errorMsg name)
    Right (rem, count, result) -> do
      failIf (name <> B8.pack " incorrect length")
             (count /= 20 || not (BL.null rem))
      return result
  _ -> left (errorMsg name)


failIf :: b -> Bool -> ContEitherT m b ()
failIf b True  = left b
failIf _ False = right ()

withMsg :: l -> Maybe a -> ContEitherT m l a
withMsg l = maybe (left l) right

missing :: B.ByteString -> B.ByteString
missing k = k <> B8.pack " is missing"

moreThanOne :: B.ByteString -> B.ByteString
moreThanOne k = k <> B8.pack " present too many times"

isSingle :: l -> [a] -> ContEitherT m l a
isSingle _ (x : []) = right x
isSingle l _        = left l

getSingleValidM
  :: B.ByteString
  -> M.Map B.ByteString [B.ByteString]
  -> ContEitherT m B.ByteString B.ByteString
getSingleValidM key mp = do
  vals <- withMsg (missing key) (M.lookup key mp)
  isSingle (moreThanOne key) vals

getValidM
  :: B.ByteString
  -> M.Map B.ByteString [B.ByteString]
  -> (B.ByteString -> B.ByteString -> ContEitherT m B.ByteString a)
  -> ContEitherT m B.ByteString a
getValidM key mp p = getSingleValidM key mp >>= p key

getSingleValidO
  :: B.ByteString
  -> M.Map B.ByteString [B.ByteString]
  -> ContEitherT m B.ByteString (Maybe B.ByteString)
getSingleValidO k m = case M.lookup k m of
  Nothing   -> return Nothing
  Just vals -> Just <$> isSingle (moreThanOne k) vals

getValidO
  :: B.ByteString
  -> M.Map B.ByteString [B.ByteString]
  -> (B.ByteString -> B.ByteString -> ContEitherT m B.ByteString a)
  -> ContEitherT m B.ByteString (Maybe a)
getValidO k m p = do
  res <- getSingleValidO k m
  case res of
    Nothing  -> return Nothing
    Just val -> Just <$> p k val
