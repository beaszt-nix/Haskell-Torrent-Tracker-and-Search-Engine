{-# LANGUAGE UnicodeSyntax #-}

module Data.Endian.Unsafe (
    BigEndian(),
    LittleEndian(),

    unsafeAssertBigEndian,
    unsafeAssertLittleEndian,

    unsafeUnwrapBigEndian,
    unsafeUnwrapLittleEndian,

    swapEndian
  ) where

import Data.Endian.Internal
import Data.Endian.Wrap

-- | put in BigEndian newtype without any swapping
unsafeAssertBigEndian    ∷ EndianSensitive α ⇒ α → BigEndian α
unsafeAssertBigEndian    = BE
{-# INLINE unsafeAssertBigEndian #-}

-- | put in LittleEndian newtype without any swapping
unsafeAssertLittleEndian ∷ EndianSensitive α ⇒ α → LittleEndian α
unsafeAssertLittleEndian = LE
{-# INLINE unsafeAssertLittleEndian #-}

-- | pull out of BigEndian newtype without any swapping
unsafeUnwrapBigEndian    ∷ EndianSensitive α ⇒ BigEndian α → α
unsafeUnwrapBigEndian    (BE a) = a
{-# INLINE unsafeUnwrapBigEndian #-}

-- | pull out of LittleEndian newtype without any swapping
unsafeUnwrapLittleEndian ∷ EndianSensitive α ⇒ LittleEndian α → α
unsafeUnwrapLittleEndian (LE a) = a
{-# INLINE unsafeUnwrapLittleEndian #-}
