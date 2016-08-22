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
  { rangeMinMax2BP      :: !(DVS.Vector Word64)
  , rangeMinMaxL0Min    :: !(DVS.Vector Int8)
  , rangeMinMaxL0Max    :: !(DVS.Vector Int8)
  , rangeMinMaxL0Excess :: !(DVS.Vector Int8)
  , rangeMinMaxL1Min    :: !(DVS.Vector Int16)
  , rangeMinMaxL1Max    :: !(DVS.Vector Int16)
  , rangeMinMaxL1Excess :: !(DVS.Vector Int16)
  }

mkRangeMinMax2 :: DVS.Vector Word64 -> RangeMinMax2
mkRangeMinMax2 bp = RangeMinMax2
  { rangeMinMax2BP = bp
  , rangeMinMaxL0Min    = rmmL0Min
  , rangeMinMaxL0Max    = rmmL0Max
  , rangeMinMaxL0Excess = rmmL0Excess
  , rangeMinMaxL1Min    = rmmL1Min
  , rangeMinMaxL1Max    = rmmL1Max
  , rangeMinMaxL1Excess = rmmL1ExcessA
  }
  where lenL0         = fromIntegral (vLength bp) :: Int
        allMinMaxL0   = DV.constructN (lenL0 + 1) genMinMaxL0
        genMinMaxL0 v = let len = DV.length v in if len == lenL0
                          then (0, 0, 0)
                          else minMaxExcess1 (bp !!! fromIntegral len)
        lenL1         = (DVS.length rmmL0Min `div` 32) + 1 :: Int
        allMinMaxL1   = DV.constructN lenL1 genMinMaxL1
        genMinMaxL1 v = let len = DV.length v in minMaxExcess1 (DVS.take 32 (DVS.drop (len * 32) bp))
        rmmL0Min      = DVS.constructN (lenL0 + 1) (\v -> let (minE, _, _)  = allMinMaxL0 DV.! DVS.length v in fromIntegral minE)
        rmmL0Max      = DVS.constructN (lenL0 + 1) (\v -> let (_, _, maxE)  = allMinMaxL0 DV.! DVS.length v in fromIntegral maxE)
        rmmL0Excess   = DVS.constructN (lenL0 + 1) (\v -> let (_, e,    _)  = allMinMaxL0 DV.! DVS.length v in fromIntegral e)
        rmmL1Min      = DVS.constructN lenL1 (\v -> let (minE, _, _)        = allMinMaxL1 DV.! DVS.length v in fromIntegral minE)
        rmmL1Max      = DVS.constructN lenL1 (\v -> let (_, _, maxE)        = allMinMaxL1 DV.! DVS.length v in fromIntegral maxE)
        rmmL1ExcessA  = DVS.constructN lenL1 (\v -> let (_, e,    _)        = allMinMaxL1 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int16

data FindState = FindBP
  | FindL0 | FindFromL0
  | FindL1 | FindFromL1

rmm2FindClose  :: RangeMinMax2 -> Int -> Count -> FindState -> Maybe Count
rmm2FindClose v s p FindBP = if v `newCloseAt` p
  then if s <= 1
    then Just p
    else rmm2FindClose v (s - 1) (p + 1) FindFromL0
  else rmm2FindClose v (s + 1) (p + 1) FindFromL0
rmm2FindClose v s p FindL0 =
  let i = p `div` 64 in
  let mins = rangeMinMaxL0Min v in
  let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindBP
    else if v `newCloseAt` p && s <= 1
      then Just p
      else  let excesses = rangeMinMaxL0Excess v in
            let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
            rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + 64) FindFromL0
rmm2FindClose v s p FindL1 =
  let !i = p `div` (64 * 32) in
  let !mins = rangeMinMaxL1Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindL0
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses = rangeMinMaxL1Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * 32)) FindFromL1
      else Nothing
rmm2FindClose v s p FindFromL0
  | p `mod` 64 == 0             = rmm2FindClose v s p FindFromL1
  | 0 <= p && p < bitLength v   = rmm2FindClose v s p FindBP
  | otherwise                   = Nothing
rmm2FindClose v s p FindFromL1
  | p `mod` (64 * 32) == 0      = if 0 <= p && p < bitLength v then rmm2FindClose v s p FindL1 else Nothing
  | 0 <= p && p < bitLength v   = rmm2FindClose v s p FindL0
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
  findOpenN         = findOpenN . rangeMinMax2BP
  {-# INLINE findOpenN   #-}

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