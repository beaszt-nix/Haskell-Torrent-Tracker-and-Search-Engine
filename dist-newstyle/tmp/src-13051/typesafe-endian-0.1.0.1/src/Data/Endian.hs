{-# LANGUAGE UnicodeSyntax, CPP #-}

module Data.Endian (
    EndianSensitive(),
    BigEndian(),
    LittleEndian(),
    toBigEndian,
    fromBigEndian,
    toLittleEndian,
    fromLittleEndian
  ) where

import Data.Endian.Internal
import Data.Endian.Wrap

#include <HsBaseConfig.h>

-- | Convert from the native format to big-endian
toBigEndian      ∷ EndianSensitive α ⇒ α → BigEndian α
-- | Convert from big-endian to the native format
fromBigEndian    ∷ EndianSensitive α ⇒ BigEndian α → α
-- | Convert from the native format to little-endian
toLittleEndian   ∷ EndianSensitive α ⇒ α → LittleEndian α
-- | Convert from little-endian to the native format
fromLittleEndian ∷ EndianSensitive α ⇒ LittleEndian α → α

beHelp ∷ EndianSensitive α ⇒ α → α
leHelp ∷ EndianSensitive α ⇒ α → α

#ifdef WORDS_BIGENDIAN
beHelp = id
leHelp = swapEndian
#else
beHelp = swapEndian
leHelp = id
#endif

toBigEndian    = BE . beHelp
toLittleEndian = LE . leHelp
fromBigEndian    (BE a) = beHelp a
fromLittleEndian (LE a) = leHelp a

{-# INLINE toBigEndian #-}
{-# INLINE fromBigEndian #-}
{-# INLINE toLittleEndian #-}
{-# INLINE fromLittleEndian #-}
