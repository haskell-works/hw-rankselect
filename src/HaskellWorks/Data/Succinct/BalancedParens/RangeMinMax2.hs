{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2
  ( RangeMinMax2(..)
  , mkRangeMinMax2
  ) where

import           Data.Int
import qualified Data.Vector                                                    as DV
import qualified Data.Vector.Storable                                           as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.BalancedParens
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.Enclose
import           HaskellWorks.Data.Succinct.BalancedParens.FindClose
import           HaskellWorks.Data.Succinct.BalancedParens.FindCloseN
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpen
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpenN
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           HaskellWorks.Data.Succinct.BalancedParens.NewCloseAt
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMax2 = RangeMinMax2
  { rangeMinMax2BP       :: !(DVS.Vector Word64)
  , rangeMinMax2L0Min    :: !(DVS.Vector Int8)
  , rangeMinMax2L0Max    :: !(DVS.Vector Int8)
  , rangeMinMax2L0Excess :: !(DVS.Vector Int8)
  , rangeMinMax2L1Min    :: !(DVS.Vector Int16)
  , rangeMinMax2L1Max    :: !(DVS.Vector Int16)
  , rangeMinMax2L1Excess :: !(DVS.Vector Int16)
  , rangeMinMax2L2Min    :: !(DVS.Vector Int16)
  , rangeMinMax2L2Max    :: !(DVS.Vector Int16)
  , rangeMinMax2L2Excess :: !(DVS.Vector Int16)
  }

mkRangeMinMax2 :: DVS.Vector Word64 -> RangeMinMax2
mkRangeMinMax2 bp = RangeMinMax2
  { rangeMinMax2BP       = bp
  , rangeMinMax2L0Min    = dvsReword rmmL0Min
  , rangeMinMax2L0Max    = dvsReword rmmL0Max
  , rangeMinMax2L0Excess = dvsReword rmmL0Excess
  , rangeMinMax2L1Min    = rmmL1Min
  , rangeMinMax2L1Max    = rmmL1Max
  , rangeMinMax2L1Excess = rmmL1Excess
  , rangeMinMax2L2Min    = rmmL2Min
  , rangeMinMax2L2Max    = rmmL2Max
  , rangeMinMax2L2Excess = rmmL2Excess
  }
  where lenBP         = fromIntegral (vLength bp) :: Int
        lenL0         = lenBP + 1
        lenL1         = (DVS.length rmmL0Min `div` 32) + 1 :: Int
        lenL2         = (DVS.length rmmL0Min `div` 1024) + 1 :: Int
        allMinMaxL0   = dvConstructNI  lenL0 (\i -> if i == lenBP then (-64, -64, 0) else minMaxExcess1 (bp !!! fromIntegral i))
        rmmL0Excess   = dvsConstructNI lenL0 (\i -> let (_, e,    _) = allMinMaxL0 DV.! i in fromIntegral e) :: DVS.Vector Int16
        rmmL1Excess   = dvsConstructNI lenL1 (\i -> DVS.foldr (+) 0 (dropTake (i * 32) 32 rmmL0Excess)) :: DVS.Vector Int16
        rmmL2Excess   = dvsConstructNI lenL1 (\i -> DVS.foldr (+) 0 (dropTake (i * 32) 32 rmmL1Excess)) :: DVS.Vector Int16
        rmmL0Min      = dvsConstructNI lenL0 (\i -> let (minE, _, _) = allMinMaxL0 DV.! i in fromIntegral minE) :: DVS.Vector Int16
        rmmL1Min      = dvsConstructNI lenL1 (\i -> genMin 0 (dropTakeFill (i * 32) 32 (-64 * 32) rmmL0Min) (dropTakeFill (i * 32) 32 (-64 * 32) rmmL0Excess))
        rmmL2Min      = dvsConstructNI lenL2 (\i -> genMin 0 (dropTakeFill (i * 32) 32 (-64 * 32 * 32) rmmL1Min) (dropTakeFill (i * 32) 32 (-64 * 32 * 32) rmmL1Excess))
        rmmL0Max      = dvsConstructNI lenL0 (\i -> let (_, _, maxE) = allMinMaxL0 DV.! i in fromIntegral maxE) :: DVS.Vector Int16
        rmmL1Max      = dvsConstructNI lenL1 (\i -> genMax 0 (dropTakeFill (i * 32) 32 0 rmmL0Max) (dropTakeFill (i * 32) 32 0 rmmL0Excess))
        rmmL2Max      = dvsConstructNI lenL2 (\i -> genMax 0 (dropTakeFill (i * 32) 32 0 rmmL1Max) (dropTakeFill (i * 32) 32 0 rmmL1Excess))
        genMin mL ms es = if not (DVS.null ms) || not (DVS.null es)
          then genMin (dvsHeadOrZero ms `min` (mL + dvsHeadOrZero es)) (DVS.tail ms) (DVS.tail es)
          else mL
        genMax mL ms es = if not (DVS.null ms) || not (DVS.null es)
          then genMax (dvsHeadOrZero ms `max` (mL + dvsHeadOrZero es)) (DVS.tail ms) (DVS.tail es)
          else mL

dropTake :: DVS.Storable a => Int -> Int -> DVS.Vector a -> DVS.Vector a
dropTake n o = DVS.take o . DVS.drop n
{-# INLINE dropTake #-}

dropTakeFill :: DVS.Storable a => Int -> Int -> a -> DVS.Vector a -> DVS.Vector a
dropTakeFill n o a v =  let r = DVS.take o (DVS.drop n v) in
                      if DVS.length r == o then r else DVS.concat [r, DVS.fromList (replicate o a)]
{-# INLINE dropTakeFill #-}

dvConstructNI :: Int -> (Int -> a) -> DV.Vector a
dvConstructNI n g = DV.constructN n (g . DV.length)
{-# INLINE dvConstructNI #-}

dvsConstructNI :: DVS.Storable a => Int -> (Int -> a) -> DVS.Vector a
dvsConstructNI n g = DVS.constructN n (g . DVS.length)
{-# INLINE dvsConstructNI #-}

dvsReword :: (DVS.Storable a, Integral a, DVS.Storable b, Num b) => DVS.Vector a -> DVS.Vector b
dvsReword v = dvsConstructNI (DVS.length v) (\i -> fromIntegral (v DVS.! i))
{-# INLINE dvsReword #-}

dvsHeadOrZero :: (DVS.Storable a, Integral a) => DVS.Vector a -> a
dvsHeadOrZero v = if not (DVS.null v) then DVS.head v else 0
{-# INLINE dvsHeadOrZero #-}

data FindState = FindBP
  | FindL0 | FindFromL0
  | FindL1 | FindFromL1
  | FindL2 | FindFromL2

rmm2FindClose  :: RangeMinMax2 -> Int -> Count -> FindState -> Maybe Count
rmm2FindClose v s p FindBP = if v `newCloseAt` p
  then if s <= 1
    then Just p
    else rmm2FindClose v (s - 1) (p + 1) FindFromL0
  else rmm2FindClose v (s + 1) (p + 1) FindFromL0
rmm2FindClose v s p FindL0 =
  let i = p `div` 64 in
  let mins = rangeMinMax2L0Min v in
  let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindBP
    else if v `newCloseAt` p && s <= 1
      then Just p
      else  let excesses = rangeMinMax2L0Excess v in
            let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
            rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + 64) FindFromL0
rmm2FindClose v s p FindL1 =
  let !i = p `div` (64 * 32) in
  let !mins = rangeMinMax2L1Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindL0
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses = rangeMinMax2L1Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * 32)) FindFromL1
      else Nothing
rmm2FindClose v s p FindL2 =
  let !i = p `div` (64 * 1024) in
  let !mins = rangeMinMax2L2Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindL1
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses = rangeMinMax2L2Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * 1024)) FindFromL2
      else Nothing
rmm2FindClose v s p FindFromL0
  | p `mod` 64 == 0             = rmm2FindClose v s p FindFromL1
  | 0 <= p && p < bitLength v   = rmm2FindClose v s p FindBP
  | otherwise                   = Nothing
rmm2FindClose v s p FindFromL1
  | p `mod` (64 * 32) == 0      = if 0 <= p && p < bitLength v then rmm2FindClose v s p FindFromL2 else Nothing
  | 0 <= p && p < bitLength v   = rmm2FindClose v s p FindL0
  | otherwise                   = Nothing
rmm2FindClose v s p FindFromL2
  | p `mod` (64 * 1024) == 0 = if 0 <= p && p < bitLength v then rmm2FindClose v s p FindL2 else Nothing
  | 0 <= p && p < bitLength v   = rmm2FindClose v s p FindL1
  | otherwise                   = Nothing
{-# INLINE rmm2FindClose #-}

instance TestBit RangeMinMax2 where
  (.?.) = (.?.) . rangeMinMax2BP
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMax2 where
  rank1 = rank1 . rangeMinMax2BP
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMax2 where
  rank0 = rank0 . rangeMinMax2BP
  {-# INLINE rank0 #-}

instance BitLength RangeMinMax2 where
  bitLength = bitLength . rangeMinMax2BP
  {-# INLINE bitLength #-}

instance OpenAt RangeMinMax2 where
  openAt = openAt . rangeMinMax2BP
  {-# INLINE openAt #-}

instance CloseAt RangeMinMax2 where
  closeAt = closeAt . rangeMinMax2BP
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMax2 where
  newCloseAt = newCloseAt . rangeMinMax2BP
  {-# INLINE newCloseAt #-}

instance FindOpenN RangeMinMax2 where
  findOpenN = findOpenN . rangeMinMax2BP
  {-# INLINE findOpenN #-}

instance FindCloseN RangeMinMax2 where
  findCloseN v s p  = (+ 1) `fmap` rmm2FindClose v (fromIntegral s) (p - 1) FindFromL0
  {-# INLINE findCloseN  #-}

instance FindClose RangeMinMax2 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindOpen RangeMinMax2 where
  findOpen = undefined
  {-# INLINE findOpen #-}

instance Enclose RangeMinMax2 where
  enclose = undefined
  {-# INLINE enclose #-}

instance BalancedParens RangeMinMax2
