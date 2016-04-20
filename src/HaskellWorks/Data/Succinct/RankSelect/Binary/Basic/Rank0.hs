{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
    ( Rank0(..)
    ) where

import qualified Data.Vector                                              as DV
import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.ElemFixedBitSize
import           HaskellWorks.Data.Bits.PopCount.PopCount0
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1 as X
import           HaskellWorks.Data.Vector.VectorLike
import           Prelude                                                  as P

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

class Rank0 v where
  rank0 :: v -> Count -> Count

deriving instance Rank0 a => Rank0 (BitShown a)

instance Rank0 Word8 where
  rank0 v s0 = s0 - rank1 v s0
  {-# INLINABLE rank0 #-}

instance Rank0 Word16 where
  rank0 v s0 = s0 - rank1 v s0
  {-# INLINABLE rank0 #-}

instance Rank0 Word32 where
  rank0 v s0 = s0 - rank1 v s0
  {-# INLINABLE rank0 #-}

instance Rank0 Word64 where
  rank0 v s0 = s0 - rank1 v s0
  {-# INLINABLE rank0 #-}

instance Rank0 [Bool] where
  rank0 = go 0
    where go r _ 0 = r
          go r (False:bs) p = go (r + 1) bs (p - 1)
          go r (True:bs) p  = go  r      bs (p - 1)
          go _ [] _         = error "Out of range"
  {-# INLINABLE rank0 #-}

instance Rank0 [Word8] where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 [Word16] where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 [Word32] where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 [Word64] where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = take (fromIntegral q) v
          maybeElem = v !! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 (DV.Vector Word8) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 (DV.Vector Word16) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 (DV.Vector Word32) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 (DV.Vector Word64) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DV.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 (DVS.Vector Word8) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 (DVS.Vector Word16) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 (DVS.Vector Word32) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank0 #-}

instance Rank0 (DVS.Vector Word64) where
  rank0 v p = popCount0 prefix + if r == 0 then 0 else (`rank0` r) maybeElem
    where (q, r)    = if p < 1 then (0, 0) else ((p - 1) `quot` elemFixedBitSize v, ((p - 1) `rem` elemFixedBitSize v) + 1)
          prefix    = DVS.take (fromIntegral q) v
          maybeElem = v !!! fromIntegral q
  {-# INLINABLE rank0 #-}
