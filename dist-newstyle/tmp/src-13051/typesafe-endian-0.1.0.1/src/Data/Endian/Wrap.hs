{-# LANGUAGE UnicodeSyntax #-}

module Data.Endian.Wrap where

-- | Wrapper, guaranteeing enclosed type is big-endian
newtype BigEndian α = BE α
-- | Wrapper, guaranteeing enclosed type is little-endian
newtype LittleEndian α = LE α
