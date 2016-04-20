{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
    ( Rank1(..)
    ) where

import qualified Data.Vector                               as DV
import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.ElemFixedBitSize
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Vector.VectorLike
import           Prelude                                   as P

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

class Rank1 v where
  rank1 :: v -> Count -> Count

deriving instance Rank1 a => Rank1 (BitShown a)

instance Rank1 Word8 where
  rank1 _ 0  = 0
  rank1 v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (8 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55) + ((r0 .>. 1) .&. 0x55)  in
    let r2 = (r1 .&. 0x33) + ((r1 .>. 2) .&. 0x33)  in
    let r3 = (r2 .&. 0x0f) + ((r2 .>. 4) .&. 0x0f)  in
    let r4 = r3 `mod` 255                           in
    Count $ fromIntegral r4
  {-# INLINABLE rank1 #-}

instance Rank1 Word16 where
  rank1 _ 0  = 0
  rank1 v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (16 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555) + ((r0 .>. 1) .&. 0x5555)  in
    let r2 = (r1 .&. 0x3333) + ((r1 .>. 2) .&. 0x3333)  in
    let r3 = (r2 .&. 0x0f0f) + ((r2 .>. 4) .&. 0x0f0f)  in
    let r4 = r3 `mod` 255                               in
    Count $ fromIntegral r4
  {-# INLINABLE rank1 #-}

instance Rank1 Word32 where
  rank1 _ 0  = 0
  rank1 v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (32 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55555555) + ((r0 .>. 1) .&. 0x55555555)  in
    let r2 = (r1 .&. 0x33333333) + ((r1 .>. 2) .&. 0x33333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f)  in
    let r4 = r3 `mod` 255                                       in
    Count $ fromIntegral r4
  {-# INLINABLE rank1 #-}

instance Rank1 Word64 where
  rank1 _ 0  = 0
  rank1 v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (64 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555555555555555) + ((r0 .>. 1) .&. 0x5555555555555555)  in
    let r2 = (r1 .&. 0x3333333333333333) + ((r1 .>. 2) .&. 0x3333333333333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f0f0f0f0f)  in
    let r4 = r3 `mod` 255                                                       in
    Count $ fromIntegral r4
  {-# INLINABLE rank1 #-}

instance Rank1 [Bool] where
  rank1 = go 0
    where go r _ 0 = r
          go r (True :bs) p = go (r + 1) bs (p - 1)
          go r (False:bs) p = go  r      bs (p - 1)
          go _ [] _         = error "Out of range"
  {-# INLINABLE rank1 #-}

instance Rank1 [Word8] where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 [Word16] where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 [Word32] where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 [Word64] where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word8) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word16) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word32) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DV.Vector Word64) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word8) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word16) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word32) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}

instance Rank1 (DVS.Vector Word64) where
  rank1 v p = popCount1 prefix + if r == 0 then 0 else (`rank1` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank1 #-}
