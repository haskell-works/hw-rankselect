{-# OPTIONS_GHC-funbox-strict-fields #-}

module HaskellWorks.Data.RankSelect.CsInterleaved
    ( CsInterleaved(..)
    , get1a
    , get2a
    , get2b
    , get2c
    , put1a
    , put2a
    , put2b
    , put2c
    ) where

import Data.Word
import Foreign.Ptr
import Foreign.Storable
import HaskellWorks.Data.Bits.BitWise

newtype CsInterleaved = CsInterleaved { unCsInterleaved :: Word64 } deriving Eq

instance Storable CsInterleaved where
  sizeOf = sizeOf . unCsInterleaved
  {-# INLINE sizeOf #-}
  alignment = alignment . unCsInterleaved
  {-# INLINE alignment #-}
  peekElemOff ptr i = CsInterleaved <$> peekElemOff (castPtr ptr) i
  {-# INLINE peekElemOff #-}
  pokeElemOff ptr i = pokeElemOff (castPtr ptr) i . unCsInterleaved
  {-# INLINE pokeElemOff #-}

get1a :: CsInterleaved -> Word64
get1a (CsInterleaved i) = (i .>. 32) .&. 0xffffffff

get2a :: CsInterleaved -> Word64
get2a (CsInterleaved i) = (i .>. 22) .&. 0x3ff

get2b :: CsInterleaved -> Word64
get2b (CsInterleaved i) = (i .>. 12) .&. 0x3ff

get2c :: CsInterleaved -> Word64
get2c (CsInterleaved i) = (i .>.  2) .&. 0x3ff

put1a :: Word64 -> CsInterleaved -> CsInterleaved
put1a v (CsInterleaved i) = CsInterleaved ((v .<. 32) .|. (i .&. 0xffffffff))

put2a :: Word64 -> CsInterleaved -> CsInterleaved
put2a v (CsInterleaved i) = CsInterleaved (((v .&. 0x3ff) .<. 22) .|. (i .&. 0xffffffff003fffff))

put2b :: Word64 -> CsInterleaved -> CsInterleaved
put2b v (CsInterleaved i) = CsInterleaved (((v .&. 0x3ff) .<. 12) .|. (i .&. 0xffffffffffc00fff))

put2c :: Word64 -> CsInterleaved -> CsInterleaved
put2c v (CsInterleaved i) = CsInterleaved (((v .&. 0x3ff) .<.  2) .|. (i .&. 0xfffffffffffff003))

instance Show CsInterleaved where
  showsPrec _ i = shows (get1a i, get2a i, get2b i, get2c i)
