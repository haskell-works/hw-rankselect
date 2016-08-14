{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L1
  ( RangeMinMaxL1(..)
  , mkRangeMinMaxL1
  ) where

import           Data.Int
import qualified Data.Vector                                              as DV
import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMaxL1 = RangeMinMaxL1
  { rangeMinMaxBP       :: DVS.Vector Word64
  , rangeMinMaxL0Min    :: DVS.Vector Int8
  , rangeMinMaxL0Max    :: DVS.Vector Int8
  , rangeMinMaxL0Excess :: DVS.Vector Int8
  , rangeMinMaxL1Min    :: DVS.Vector Int16
  , rangeMinMaxL1Max    :: DVS.Vector Int16
  , rangeMinMaxL1Excess :: DVS.Vector Int16
  }

wordsL1 :: Int
wordsL1 = 8
{-# INLINE wordsL1 #-}

bitsL1 :: Count
bitsL1 = fromIntegral (wordsL1 * 64)

mkRangeMinMaxL1 :: DVS.Vector Word64 -> RangeMinMaxL1
mkRangeMinMaxL1 bp = RangeMinMaxL1
  { rangeMinMaxBP       = bp
  , rangeMinMaxL0Min    = DVS.constructN lenL0 (\v -> let (minE, _, _) = allMinMaxL0 DV.! DVS.length v in fromIntegral minE)
  , rangeMinMaxL0Max    = DVS.constructN lenL0 (\v -> let (_, _, maxE) = allMinMaxL0 DV.! DVS.length v in fromIntegral maxE)
  , rangeMinMaxL0Excess = rangeMinMaxL0ExcessA
  , rangeMinMaxL1Min    = DVS.constructN lenL1 (\v -> let (minE, _, _) = allMinMaxL1 DV.! DVS.length v in fromIntegral minE)
  , rangeMinMaxL1Max    = DVS.constructN lenL1 (\v -> let (_, _, maxE) = allMinMaxL1 DV.! DVS.length v in fromIntegral maxE)
  , rangeMinMaxL1Excess = rangeMinMaxL1ExcessA
  }
  where lenBP         = fromIntegral (vLength bp) :: Int
        lenL0         = lenBP + 1
        allMinMaxL0   = DV.constructN lenL0 genMinMaxL0
        genMinMaxL0 v = let doneLen = DV.length v in if doneLen == lenBP
                          then (0, 0, 0)
                          else minMaxExcess1 (bp !!! fromIntegral doneLen)
        lenL1         = fromIntegral (vLength bp) `div` wordsL1 + 1 :: Int
        allMinMaxL1   = DV.constructN lenL1 genMinMaxL1
        genMinMaxL1 v = let len = DV.length v in
                        minMaxExcess1 (DVS.take wordsL1 (DVS.drop (fromIntegral len * wordsL1) bp))
        rangeMinMaxL0ExcessA = DVS.constructN lenL0 (\v -> let (_, e,    _) = allMinMaxL0 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int8
        rangeMinMaxL1ExcessA = DVS.constructN lenL1 (\v -> let (_, e,    _) = allMinMaxL1 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int16

instance TestBit RangeMinMaxL1 where
  (.?.) = (.?.) . rangeMinMaxBP
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMaxL1 where
  rank1 = rank1 . rangeMinMaxBP
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMaxL1 where
  rank0 = rank0 . rangeMinMaxBP
  {-# INLINE rank0 #-}

instance BitLength RangeMinMaxL1 where
  bitLength = bitLength . rangeMinMaxBP
  {-# INLINE bitLength #-}

resultToMaybe :: RangeMinMaxResult a -> Maybe a
resultToMaybe Fail          = Nothing
resultToMaybe NoSkip        = Nothing
resultToMaybe (Progress a)  = Just a

findCloseN' :: RangeMinMaxL1 -> Int -> Count -> RangeMinMaxResult Count
findCloseN' v s p = if v `closeAt` p
  then if s <= 1
    then Progress p
    else rangeMinMaxFindCloseN v (s - 1) (p + 1)
  else rangeMinMaxFindCloseN v (s + 1) (p + 1)
{-# INLINE findCloseN' #-}

rangeMinMaxFindCloseN :: RangeMinMaxL1 -> Int -> Count -> RangeMinMaxResult Count
rangeMinMaxFindCloseN v s p = if 0 < p && p <= bitLength v
  then if (p - 1) `mod` elemBitLength (rangeMinMaxBP v) /= 0
    then findCloseN' v s p
    else if (p - 1) `mod` bitsL1 /= 0
      then rangeMinMaxFindCloseNL0 v s p <||> findCloseN' v s p
      else rangeMinMaxFindCloseNL1 v s p <||> rangeMinMaxFindCloseNL0 v s p <||> findCloseN' v s p
  else Fail
{-# INLINE rangeMinMaxFindCloseN #-}

rangeMinMaxFindCloseNL0 :: RangeMinMaxL1 -> Int -> Count -> RangeMinMaxResult Count
rangeMinMaxFindCloseNL0 v s p =
  let i = (p - 1) `div` elemBitLength bp in
  let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then NoSkip
    else if v `closeAt` p && s <= 1
      then Progress p
      else let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
            rangeMinMaxFindCloseN  v (fromIntegral (excess + fromIntegral s)) (p + 64)
  where bp                    = rangeMinMaxBP v
        mins                  = rangeMinMaxL0Min v
        excesses              = rangeMinMaxL0Excess v
{-# INLINE rangeMinMaxFindCloseNL0 #-}

rangeMinMaxFindCloseNL1 :: RangeMinMaxL1 -> Int -> Count -> RangeMinMaxResult Count
rangeMinMaxFindCloseNL1 v s p =
  let i = (p - 1) `div` bitsL1 in
  let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then  NoSkip
    else if v `closeAt` p && s <= 1
      then Progress p
      else let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
            rangeMinMaxFindCloseN  v (fromIntegral (excess + fromIntegral s)) (p + bitsL1)
  where mins                  = rangeMinMaxL1Min v
        excesses              = rangeMinMaxL1Excess v
{-# INLINE rangeMinMaxFindCloseNL1 #-}

instance BalancedParens RangeMinMaxL1 where
  openAt            = openAt      . rangeMinMaxBP
  closeAt           = closeAt     . rangeMinMaxBP
  -- findOpenN         = findOpenN   . rangeMinMaxBP
  findCloseN v s p  = resultToMaybe (rangeMinMaxFindCloseN v (fromIntegral s) p)

  {-# INLINE openAt      #-}
  {-# INLINE closeAt     #-}
  -- {-# INLINE findOpenN   #-}
  {-# INLINE findCloseN  #-}
