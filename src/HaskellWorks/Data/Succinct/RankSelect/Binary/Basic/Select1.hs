{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
    ( Select1(..)
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

class Select1 v where
  select1 :: v -> Count -> Count

deriving instance Select1 a => Select1 (BitShown a)

-- TODO: Implement NOT interms of select for word-16
instance Select1 Word8 where
  select1 _ 0 = 0
  select1 v p = select1 (fromIntegral v :: Word16) p
  {-# INLINABLE select1 #-}

-- TODO: Remove redundant code to optimise
instance Select1 Word16 where
  select1 _ 0 = 0
  select1 v rn =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x5555) + ((v .>.  1) .&. 0x5555)    in
    let b = (a .&. 0x3333) + ((a .>.  2) .&. 0x3333)    in
    let c = (b .&. 0x0f0f) + ((b .>.  4) .&. 0x0f0f)    in
    let d = (c .&. 0x00ff) + ((c .>.  8) .&. 0x00ff)    in
    -- Now do branchless select!
    let r0 = d + 1 - (fromIntegral (getCount rn) :: Word16)                     in
    let s0 = 64 :: Word16                                                       in
    let t0 = (d .>. 32) + (d .>. 48)                                            in
    let s1 = s0 - ((t0 - r0) .&. 256) .>. 3                                     in
    let r1 = r0 - (t0 .&. ((t0 - r0) .>. 8))                                    in
    let t1 =      (d .>. fromIntegral (s1 - 16)) .&. 0xff                       in
    let s2 = s1 - ((t1 - r1) .&. 256) .>. 4                                     in
    let r2 = r1 - (t1 .&. ((t1 - r1) .>. 8))                                    in
    let t2 =      (c .>. fromIntegral (s2 - 8))  .&. 0xf                        in
    let s3 = s2 - ((t2 - r2) .&. 256) .>. 5                                     in
    let r3 = r2 - (t2 .&. ((t2 - r2) .>. 8))                                    in
    let t3 =      (b .>. fromIntegral (s3 - 4))  .&. 0x7                        in
    let s4 = s3 - ((t3 - r3) .&. 256) .>. 6                                     in
    let r4 = r3 - (t3 .&. ((t3 - r3) .>. 8))                                    in
    let t4 =      (a .>. fromIntegral (s4 - 2))  .&. 0x3                        in
    let s5 = s4 - ((t4 - r4) .&. 256) .>. 7                                     in
    let r5 = r4 - (t4 .&. ((t4 - r4) .>. 8))                                    in
    let t5 =      (v .>. fromIntegral (s5 - 1))  .&. 0x1                        in
    let s6 = s5 - ((t5 - r5) .&. 256) .>. 8                                     in
    fromIntegral s6
  {-# INLINABLE select1 #-}

-- TODO: Remove redundant code to optimise
instance Select1 Word32 where
  select1 _ 0 = 0
  select1 v rn =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x55555555) + ((v .>.  1) .&. 0x55555555)    in
    let b = (a .&. 0x33333333) + ((a .>.  2) .&. 0x33333333)    in
    let c = (b .&. 0x0f0f0f0f) + ((b .>.  4) .&. 0x0f0f0f0f)    in
    let d = (c .&. 0x00ff00ff) + ((c .>.  8) .&. 0x00ff00ff)    in
    let e = (d .&. 0x000000ff) + ((d .>. 16) .&. 0x000000ff)    in
    -- Now do branchless select!
    let r0 = e + 1 - (fromIntegral (getCount rn) :: Word32)                     in
    let s0 = 64 :: Word32                                                       in
    let t0 = (d .>. 32) + (d .>. 48)                                            in
    let s1 = s0 - ((t0 - r0) .&. 256) .>. 3                                     in
    let r1 = r0 - (t0 .&. ((t0 - r0) .>. 8))                                    in
    let t1 =      (d .>. fromIntegral (s1 - 16)) .&. 0xff                       in
    let s2 = s1 - ((t1 - r1) .&. 256) .>. 4                                     in
    let r2 = r1 - (t1 .&. ((t1 - r1) .>. 8))                                    in
    let t2 =      (c .>. fromIntegral (s2 - 8))  .&. 0xf                        in
    let s3 = s2 - ((t2 - r2) .&. 256) .>. 5                                     in
    let r3 = r2 - (t2 .&. ((t2 - r2) .>. 8))                                    in
    let t3 =      (b .>. fromIntegral (s3 - 4))  .&. 0x7                        in
    let s4 = s3 - ((t3 - r3) .&. 256) .>. 6                                     in
    let r4 = r3 - (t3 .&. ((t3 - r3) .>. 8))                                    in
    let t4 =      (a .>. fromIntegral (s4 - 2))  .&. 0x3                        in
    let s5 = s4 - ((t4 - r4) .&. 256) .>. 7                                     in
    let r5 = r4 - (t4 .&. ((t4 - r4) .>. 8))                                    in
    let t5 =      (v .>. fromIntegral (s5 - 1))  .&. 0x1                        in
    let s6 = s5 - ((t5 - r5) .&. 256) .>. 8                                     in
    fromIntegral s6
  {-# INLINABLE select1 #-}

instance Select1 Word64 where
  select1 _ 0 = 0
  select1 v rn =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x5555555555555555) + ((v .>.  1) .&. 0x5555555555555555)    in
    let b = (a .&. 0x3333333333333333) + ((a .>.  2) .&. 0x3333333333333333)    in
    let c = (b .&. 0x0f0f0f0f0f0f0f0f) + ((b .>.  4) .&. 0x0f0f0f0f0f0f0f0f)    in
    let d = (c .&. 0x00ff00ff00ff00ff) + ((c .>.  8) .&. 0x00ff00ff00ff00ff)    in
    let e = (d .&. 0x0000ffff0000ffff) + ((d .>. 16) .&. 0x0000ffff0000ffff)    in
    let f = (e .&. 0x00000000ffffffff) + ((e .>. 32) .&. 0x00000000ffffffff)    in
    -- Now do branchless select!
    let r0 = f + 1 - fromIntegral (getCount rn) :: Word64                       in
    let s0 = 64 :: Word64                                                       in
    let t0 = (d .>. 32) + (d .>. 48)                                            in
    let s1 = s0 - ((t0 - r0) .&. 256) .>. 3                                     in
    let r1 = r0 - (t0 .&. ((t0 - r0) .>. 8))                                    in
    let t1 =      (d .>. fromIntegral (s1 - 16)) .&. 0xff                       in
    let s2 = s1 - ((t1 - r1) .&. 256) .>. 4                                     in
    let r2 = r1 - (t1 .&. ((t1 - r1) .>. 8))                                    in
    let t2 =      (c .>. fromIntegral (s2 - 8))  .&. 0xf                        in
    let s3 = s2 - ((t2 - r2) .&. 256) .>. 5                                     in
    let r3 = r2 - (t2 .&. ((t2 - r2) .>. 8))                                    in
    let t3 =      (b .>. fromIntegral (s3 - 4))  .&. 0x7                        in
    let s4 = s3 - ((t3 - r3) .&. 256) .>. 6                                     in
    let r4 = r3 - (t3 .&. ((t3 - r3) .>. 8))                                    in
    let t4 =      (a .>. fromIntegral (s4 - 2))  .&. 0x3                        in
    let s5 = s4 - ((t4 - r4) .&. 256) .>. 7                                     in
    let r5 = r4 - (t4 .&. ((t4 - r4) .>. 8))                                    in
    let t5 =      (v .>. fromIntegral (s5 - 1))  .&. 0x1                        in
    let s6 = s5 - ((t5 - r5) .&. 256) .>. 8                                     in
    fromIntegral s6
  {-# INLINABLE select1 #-}

instance Select1 [Bool] where
  select1 = go 0
    where go r _ 0 = r
          go r (True :bs) c = go (r + 1) bs (c - 1)
          go r (False:bs) c = go (r + 1) bs  c
          go _ []         _ = error "Out of range"
  {-# INLINABLE select1 #-}

instance Select1 [Word8] where
  select1 v c = go v c 0
    where go :: [Word8] -> Count -> Count -> Count
          go _ 0  acc = acc
          go u d acc = let w = head u in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (tail u) (d - pc) (acc + elemFixedBitSize u)
  {-# INLINABLE select1 #-}

instance Select1 [Word16] where
  select1 v c = go v c 0
    where go :: [Word16] -> Count -> Count -> Count
          go _ 0  acc = acc
          go u d acc = let w = head u in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (tail u) (d - pc) (acc + elemFixedBitSize u)
  {-# INLINABLE select1 #-}

instance Select1 [Word32] where
  select1 v c = go v c 0
    where go :: [Word32] -> Count -> Count -> Count
          go _ 0  acc = acc
          go u d acc = let w = head u in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (tail u) (d - pc) (acc + elemFixedBitSize u)
  {-# INLINABLE select1 #-}

instance Select1 [Word64] where
  select1 v c = go v c 0
    where go :: [Word64] -> Count -> Count -> Count
          go _ 0  acc = acc
          go u d acc = let w = head u in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (tail u) (d - pc) (acc + elemFixedBitSize u)
  {-# INLINABLE select1 #-}

instance Select1 (DVS.Vector Word8) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select1 #-}

instance Select1 (DVS.Vector Word16) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select1 #-}

instance Select1 (DVS.Vector Word32) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select1 #-}

instance Select1 (DVS.Vector Word64) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select1 #-}

instance Select1 (DV.Vector Word8) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select1 #-}

instance Select1 (DV.Vector Word16) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select1 #-}

instance Select1 (DV.Vector Word32) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select1 #-}

instance Select1 (DV.Vector Word64) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + elemFixedBitSize v)
  {-# INLINABLE select1 #-}
