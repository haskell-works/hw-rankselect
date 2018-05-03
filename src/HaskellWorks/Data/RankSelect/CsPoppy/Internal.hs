{-# OPTIONS_GHC-funbox-strict-fields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}

module HaskellWorks.Data.RankSelect.CsPoppy.Internal
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
    , makeCsPoppyBlocks1
    , makeCsPoppyBlocks2
    ) where

import Data.Word
import Foreign.Ptr
import Foreign.Storable
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1

import qualified Data.Vector.Storable as DVS

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

makeCsPoppyBlocks1 :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyBlocks1 v = DVS.constructN (((DVS.length v + 8 - 1) `div` 8) + 1) genBlocks
  where genBlocks :: DVS.Vector Word64 -> Word64
        genBlocks u = let i = DVS.length u in popCount1 (DVS.take 8 (DVS.drop (i * 8) v))

makeCsPoppyBlocks2 :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyBlocks2 v = DVS.constructN (((DVS.length v + 8 - 1) `div` 8) + 1) genBlocks
  where e = DVS.length v
        genBlocks :: DVS.Vector Word64 -> Word64
        genBlocks u = let i = DVS.length u in if
          | (i + 1) * 8 <= e -> let j = i * 8 in
              popCount1 (DVS.unsafeIndex v (j + 0)) +
              popCount1 (DVS.unsafeIndex v (j + 1)) +
              popCount1 (DVS.unsafeIndex v (j + 2)) +
              popCount1 (DVS.unsafeIndex v (j + 3)) +
              popCount1 (DVS.unsafeIndex v (j + 4)) +
              popCount1 (DVS.unsafeIndex v (j + 5)) +
              popCount1 (DVS.unsafeIndex v (j + 6)) +
              popCount1 (DVS.unsafeIndex v (j + 7))
          | otherwise -> popCount1 (DVS.take 8 (DVS.drop (i * 8) v))
