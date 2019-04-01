{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

module HaskellWorks.Data.RankSelect.CsPoppy.Internal.Lookup where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import Prelude                        hiding (drop, length, pi, take)

import qualified Data.Vector.Storable as DVS

binarySearchPBounds :: (Word64 -> Bool) -> DVS.Vector Word64 -> Word64 -> Word64 -> Word64
binarySearchPBounds p v = loop
  where loop :: Word64 -> Word64 -> Word64
        loop !l !u
          | u <= l    = l
          | otherwise = let e = v !!! fromIntegral k in if p e then loop l k else loop (k + 1) u
          where k = (u + l) .>. 1
{-# INLINE binarySearchPBounds #-}

lookupLayerMXFrom :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> Word64
lookupLayerMXFrom prev i r v | i < length v =
  let mw  = v !!! fromIntegral i                      :: Word64
      mx  =      ( mw .&. 0x00000000ffffffff        ) :: Word64
  in if r <= mx
    then prev
    else lookupLayerMXFrom i (i + 1) r v
lookupLayerMXFrom prev _ _ _ = prev
{-# INLINE lookupLayerMXFrom #-}

lookupLayerMFrom1 :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> (Word64, Word64)
lookupLayerMFrom1 i _ r v
  | r <= mx   = error "This shouldn't happen"
  | r <= ma   = (mx, j * 4    )
  | r <= mb   = (ma, j * 4 + 1)
  | r <= mc   = (mb, j * 4 + 2)
  | otherwise = (mc, j * 4 + 3)
  where j   = lookupLayerMXFrom 0 i r v
        mw  = v !!! fromIntegral j                      :: Word64
        mx  =      ( mw .&. 0x00000000ffffffff        ) :: Word64
        ma  = mx + ((mw .&. 0x000003ff00000000) .>. 32) :: Word64
        mb  = ma + ((mw .&. 0x000ffc0000000000) .>. 42) :: Word64
        mc  = mb + ((mw .&. 0x3ff0000000000000) .>. 52) :: Word64
{-# INLINE lookupLayerMFrom1 #-}

lookupLayerMFrom2 :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> (Word64, Word64)
lookupLayerMFrom2 i j r v =
  let cmp w = (r - 1) < (w .&. 0xffffffff)
      !k  = (binarySearchPBounds cmp v i j - 1) `max` 0
      !mw = v !!! fromIntegral k                      :: Word64
      !mx =      ( mw .&. 0x00000000ffffffff        ) :: Word64
      !ma = mx + ((mw .&. 0x000003ff00000000) .>. 32) :: Word64
      !mb = ma + ((mw .&. 0x000ffc0000000000) .>. 42) :: Word64
      !mc = mb + ((mw .&. 0x3ff0000000000000) .>. 52) :: Word64
  in if
    | r <= mx   -> (0 , 0        )
    | r <= ma   -> (mx, k * 4    )
    | r <= mb   -> (ma, k * 4 + 1)
    | r <= mc   -> (mb, k * 4 + 2)
    | otherwise -> (mc, k * 4 + 3)
{-# INLINE lookupLayerMFrom2 #-}
