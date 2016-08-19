{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Succinct.BalancedParens.Broadword
  ( findCloseW64
  ) where

import           Data.Word
-- import           Debug.Trace
-- import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.Broadword
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal

newtype Fast a = Fast a

fast :: Fast Word64 -> Word64
fast (Fast a) = a

-- traceW :: String -> Word64 -> Word64
-- traceW s w = trace (s ++ ": " ++ show (BitShown w) ++ " : " ++ show w) w

findCloseW64 :: Word64 -> Word64
findCloseW64 x =
  let !b00 = x - ((x .&. 0xaaaaaaaaaaaaaaaa) .>. 1)                                   in -- let !_ = traceW "b00" b00 in
  let !b01 = (b00 .&. 0x3333333333333333) + ((b00 .>. 2) .&. 0x3333333333333333)      in -- let !_ = traceW "b01" b01 in
  let !b02 = (b01 + (b01 .>. 4)) .&. 0x0f0f0f0f0f0f0f0f                               in -- let !_ = traceW "b02" b02 in
  let !b03 = (b02 * 0x0101010101010101) .<. 1                                         in -- let !_ = traceW "b03" b03 in
  let !b04 = kBitDiff 8 (h 8 .|. 0x4038302820181008) b03                              in -- let !_ = traceW "b04" b04 in
  let !u00 = (((((b04 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                  in -- let !_ = traceW "u00" u00 in
  let !z00 = ((h 8 .>. 1) .|. (l 8 * 7)) .&. u00                                      in -- let !_ = traceW "z00" z00 in
  let !b10 = b04 - (l 8 * 2 - ((x .>. 6 .&. l 8 .<. 1) + (x .>. 5 .&. l 8 .<. 1)))    in -- let !_ = traceW "b10" b10 in
  let !u10 = (((((b10 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                  in -- let !_ = traceW "u10" u10 in
  let !z10 = (z00 .&. comp u10) .|. (((h 8 .>. 1) .|. (l 8 * 5)) .&. u10)             in -- let !_ = traceW "z10" z10 in
  let !b20 = b10 - (l 8 * 2 - ((x .>. 4 .&. l 8 .<. 1) + (x .>. 3 .&. l 8 .<. 1)))    in -- let !_ = traceW "b20" b20 in
  let !u20 = (((((b20 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                  in -- let !_ = traceW "u20" u20 in
  let !z20 = (z10 .&. comp u20) .|. (((h 8 .>. 1) .|. (l 8 * 3)) .&. u20)             in -- let !_ = traceW "z20" z20 in
  let !b30 = b10 - (l 8 * 2 - ((x .>. 2 .&. l 8 .<. 1) + (x .>. 1 .&. l 8 .<. 1)))    in -- let !_ = traceW "b30" b30 in
  let !u30 = (((((b30 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                  in -- let !_ = traceW "u30" u30 in
  let !z30 = (z20 .&. comp u30) .|. (((h 8 .>. 1) .|.  l 8     ) .&. u30)             in -- let !_ = traceW "z30" z30 in
  let !p00 = lsb (z30 .>. 6 .&. l 8)                                                  in
  let !r00 = ((p00 + ((z30 .>. fromIntegral p00) .&. 0x3f)) .|. (p00 .>. 8)) .&. 0x7f in
  r00

instance OpenAt (Fast Word64) where
  openAt :: Fast Word64 -> Count -> Bool
  openAt = openAt . fast
  {-# INLINE openAt #-}

instance CloseAt (Fast Word64) where
  closeAt :: Fast Word64 -> Count -> Bool
  closeAt = closeAt . fast
  {-# INLINE closeAt #-}

-- instance BalancedParens (Fast Word64) where
--   findOpenN   :: v -> Count -> Count -> Maybe Count
--   findCloseN  :: v -> Count -> Count -> Maybe Count
