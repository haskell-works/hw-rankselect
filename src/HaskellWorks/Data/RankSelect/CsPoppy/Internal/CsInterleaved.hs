{-# OPTIONS_GHC-funbox-strict-fields #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.CsPoppy.Internal.CsInterleaved
    ( CsInterleaved(..)
    , getCsiX
    , getCsiA
    , getCsiB
    , getCsiC
    , getCsiTotal
    , mkCsi
    , putCsiX
    , putCsiA
    , putCsiB
    , putCsiC
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

mkCsi :: Word64 -> Word64 -> Word64 -> Word64 -> CsInterleaved
mkCsi x a b c = CsInterleaved
    $   ((x .&. 0xffffffff) .<.  0)
    .|. ((a .&.      0x3ff) .<. 32)
    .|. ((b .&.      0x3ff) .<. 42)
    .|. ((c .&.      0x3ff) .<. 52)

getCsiX :: CsInterleaved -> Word64
getCsiX (CsInterleaved i) = i .&. 0xffffffff

getCsiA :: CsInterleaved -> Word64
getCsiA (CsInterleaved i) = (i .>. 32) .&. 0x3ff

getCsiB :: CsInterleaved -> Word64
getCsiB (CsInterleaved i) = (i .>. 42) .&. 0x3ff

getCsiC :: CsInterleaved -> Word64
getCsiC (CsInterleaved i) = (i .>. 52) .&. 0x3ff

getCsiTotal :: CsInterleaved -> Word64
getCsiTotal csi = getCsiX csi + getCsiA csi + getCsiB csi + getCsiC csi

putCsiX :: Word64 -> CsInterleaved -> CsInterleaved
putCsiX v (CsInterleaved i) = CsInterleaved (((v .&. 0xffffffff) .<.  0) .|. (i .&. 0xffffffff00000000))

putCsiA :: Word64 -> CsInterleaved -> CsInterleaved
putCsiA v (CsInterleaved i) = CsInterleaved (((v .&.      0x3ff) .<. 32) .|. (i .&. 0xfffffc00ffffffff))

putCsiB :: Word64 -> CsInterleaved -> CsInterleaved
putCsiB v (CsInterleaved i) = CsInterleaved (((v .&.      0x3ff) .<. 42) .|. (i .&. 0xfff003ffffffffff))

putCsiC :: Word64 -> CsInterleaved -> CsInterleaved
putCsiC v (CsInterleaved i) = CsInterleaved (((v .&.      0x3ff) .<. 52) .|. (i .&. 0xc00fffffffffffff))

instance Show CsInterleaved where
  showsPrec _ i = shows (getCsiX i, getCsiA i, getCsiB i, getCsiC i)
