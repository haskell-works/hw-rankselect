{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L2
  ( RangeMinMaxL2(..)
  , mkRangeMinMaxL2
  ) where

import           Data.Int
import qualified Data.Vector                                              as DV
import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMaxL2 = RangeMinMaxL2
  { rangeMinMaxBP       :: DVS.Vector Word64
  , rangeMinMaxL0Min    :: DVS.Vector Int8
  , rangeMinMaxL0Max    :: DVS.Vector Int8
  , rangeMinMaxL0Excess :: DVS.Vector Int8
  , rangeMinMaxL1Min    :: DVS.Vector Int16
  , rangeMinMaxL1Max    :: DVS.Vector Int16
  , rangeMinMaxL1Excess :: DVS.Vector Int16
  , rangeMinMaxL2Min    :: DVS.Vector Int16
  , rangeMinMaxL2Max    :: DVS.Vector Int16
  , rangeMinMaxL2Excess :: DVS.Vector Int16
  }

-- wordsL0 :: Int
-- wordsL0 = 8
-- {-# INLINE wordsL0 #-}

wordsL1 :: Int
wordsL1 = 8
{-# INLINE wordsL1 #-}

wordsL2 :: Int
wordsL2 = 16
{-# INLINE wordsL2 #-}

bitsL1 :: Count
bitsL1 = fromIntegral (wordsL1 * 64)

bitsL2 :: Count
bitsL2 = fromIntegral (wordsL2 * 64)

mkRangeMinMaxL2 :: DVS.Vector Word64 -> RangeMinMaxL2
mkRangeMinMaxL2 bp = RangeMinMaxL2
  { rangeMinMaxBP       = bp
  , rangeMinMaxL0Min    = DVS.constructN lenL0 (\v -> let (minE, _, _) = allMinMaxL0 DV.! DVS.length v in fromIntegral minE)
  , rangeMinMaxL0Max    = DVS.constructN lenL0 (\v -> let (_, _, maxE) = allMinMaxL0 DV.! DVS.length v in fromIntegral maxE)
  , rangeMinMaxL0Excess = rangeMinMaxL0ExcessA
  , rangeMinMaxL1Min    = DVS.constructN lenL1 (\v -> let (minE, _, _) = allMinMaxL1 DV.! DVS.length v in fromIntegral minE)
  , rangeMinMaxL1Max    = DVS.constructN lenL1 (\v -> let (_, _, maxE) = allMinMaxL1 DV.! DVS.length v in fromIntegral maxE)
  , rangeMinMaxL1Excess = rangeMinMaxL1ExcessA
  , rangeMinMaxL2Min    = DVS.constructN lenL2 (\v -> let (minE, _, _) = allMinMaxL2 DV.! DVS.length v in fromIntegral minE)
  , rangeMinMaxL2Max    = DVS.constructN lenL2 (\v -> let (_, _, maxE) = allMinMaxL2 DV.! DVS.length v in fromIntegral maxE)
  , rangeMinMaxL2Excess = rangeMinMaxL2ExcessB
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
        lenL2         = fromIntegral (vLength bp) `div` wordsL2 + 1 :: Int
        allMinMaxL2   = DV.constructN lenL2 genMinMaxL2
        genMinMaxL2 v = let len = DV.length v in
                        minMaxExcess1 (DVS.take wordsL2 (DVS.drop (fromIntegral len * wordsL2) bp))
        rangeMinMaxL0ExcessA = DVS.constructN lenL0 (\v -> let (_, e,    _) = allMinMaxL0 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int8
        rangeMinMaxL1ExcessA = DVS.constructN lenL1 (\v -> let (_, e,    _) = allMinMaxL1 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int16
        rangeMinMaxL2ExcessB = DVS.constructN lenL2 (\v -> DVS.foldr (+)                  0 (takeDrop16 bitsRatioL2 (DVS.length v * bitsRatioL2) rangeMinMaxL1ExcessA)) :: DVS.Vector Int16

bitsRatioL2 :: Int
bitsRatioL2 = wordsL2 `div` wordsL1

takeDrop16 :: Int -> Int -> DVS.Vector Int16 -> DVS.Vector Int16
takeDrop16 n o v = DVS.take n (DVS.drop o v)

instance TestBit RangeMinMaxL2 where
  (.?.) = (.?.) . rangeMinMaxBP
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMaxL2 where
  rank1 = rank1 . rangeMinMaxBP
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMaxL2 where
  rank0 = rank0 . rangeMinMaxBP
  {-# INLINE rank0 #-}

instance BitLength RangeMinMaxL2 where
  bitLength = bitLength . rangeMinMaxBP
  {-# INLINE bitLength #-}

data Result a = Progress a | NoSkip | Fail
  deriving (Eq, Show)

(<||>) :: Result a -> Result a -> Result a
Fail        <||> _          = Fail
NoSkip      <||> b          = b
Progress a  <||> _          = Progress a

resultToMaybe :: Result a -> Maybe a
resultToMaybe Fail          = Nothing
resultToMaybe NoSkip        = Nothing
resultToMaybe (Progress a)  = Just a

findCloseN' :: RangeMinMaxL2 -> Int -> Count -> Result Count
findCloseN' v s p = if v `closeAt` p
  then if s <= 1
    then Progress p
    else rangeMinMaxFindCloseN v (s - 1) (p + 1)
  else rangeMinMaxFindCloseN v (s + 1) (p + 1)
{-# INLINE findCloseN' #-}

rangeMinMaxFindCloseN :: RangeMinMaxL2 -> Int -> Count -> Result Count
rangeMinMaxFindCloseN v s p = if 0 < p && p <= bitLength v
  then if (p - 1) `mod` elemBitLength (rangeMinMaxBP v) /= 0
    then findCloseN' v s p
    else if (p - 1) `mod` bitsL1 /= 0
      then rangeMinMaxFindCloseNL0 v s p <||> findCloseN' v s p
      else if (p - 1) `mod` bitsL2 /= 0
        then rangeMinMaxFindCloseNL1 v s p <||> rangeMinMaxFindCloseNL0 v s p <||> findCloseN' v s p
        else rangeMinMaxFindCloseNL2 v s p <||> rangeMinMaxFindCloseNL1 v s p <||> rangeMinMaxFindCloseNL0 v s p <||> findCloseN' v s p
  else Fail
{-# INLINE rangeMinMaxFindCloseN #-}

rangeMinMaxFindCloseNL0 :: RangeMinMaxL2 -> Int -> Count -> Result Count
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

rangeMinMaxFindCloseNL1 :: RangeMinMaxL2 -> Int -> Count -> Result Count
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


rangeMinMaxFindCloseNL2 :: RangeMinMaxL2 -> Int -> Count -> Result Count
rangeMinMaxFindCloseNL2 v s p =
  let i = (p - 1) `div` bitsL2 in
  let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then  NoSkip
    else if v `closeAt` p && s <= 1
      then Progress p
      else let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
            rangeMinMaxFindCloseN  v (fromIntegral (excess + fromIntegral s)) (p + bitsL2)
  where mins                  = rangeMinMaxL2Min v
        excesses              = rangeMinMaxL2Excess v
{-# INLINE rangeMinMaxFindCloseNL2 #-}

instance BalancedParens RangeMinMaxL2 where
  openAt            = openAt      . rangeMinMaxBP
  closeAt           = closeAt     . rangeMinMaxBP
  -- findOpenN         = findOpenN   . rangeMinMaxBP
  findCloseN v s p  = resultToMaybe (rangeMinMaxFindCloseN v (fromIntegral s) p)

  {-# INLINE openAt      #-}
  {-# INLINE closeAt     #-}
  -- {-# INLINE findOpenN   #-}
  {-# INLINE findCloseN  #-}
