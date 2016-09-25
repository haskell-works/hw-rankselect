{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax
  ( RangeMinMax(..)
  , mkRangeMinMax
  ) where

import           Data.Int
import qualified Data.Vector                                                    as DV
import qualified Data.Vector.Storable                                           as DVS
import           Data.Word
import           HaskellWorks.Data.AtIndex
import           HaskellWorks.Data.Bits.AllExcess.AllExcess1
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
import           Prelude hiding (length)

data RangeMinMax = RangeMinMax
  { rangeMinMaxBP       :: !(DVS.Vector Word64)
  , rangeMinMaxL0Min    :: !(DVS.Vector Int8)
  , rangeMinMaxL0Max    :: !(DVS.Vector Int8)
  , rangeMinMaxL0Excess :: !(DVS.Vector Int8)
  , rangeMinMaxL1Min    :: !(DVS.Vector Int16)
  , rangeMinMaxL1Max    :: !(DVS.Vector Int16)
  , rangeMinMaxL1Excess :: !(DVS.Vector Int16)
  , rangeMinMaxL2Min    :: !(DVS.Vector Int16)
  , rangeMinMaxL2Max    :: !(DVS.Vector Int16)
  , rangeMinMaxL2Excess :: !(DVS.Vector Int16)
  }

factorL0 :: Integral a => a
factorL0 = 1
{-# INLINE factorL0 #-}

factorL1 :: Integral a => a
factorL1 = 32
{-# INLINE factorL1 #-}

factorL2 :: Integral a => a
factorL2 = 32
{-# INLINE factorL2 #-}

pageSizeL0 :: Integral a => a
pageSizeL0 = factorL0
{-# INLINE pageSizeL0 #-}

pageSizeL1 :: Integral a => a
pageSizeL1 = pageSizeL0 * factorL1
{-# INLINE pageSizeL1 #-}

pageSizeL2 :: Integral a => a
pageSizeL2 = pageSizeL1 * factorL2
{-# INLINE pageSizeL2 #-}

mkRangeMinMax :: DVS.Vector Word64 -> RangeMinMax
mkRangeMinMax bp = RangeMinMax
  { rangeMinMaxBP       = bp
  , rangeMinMaxL0Min    = rmmL0Min
  , rangeMinMaxL0Max    = rmmL0Max
  , rangeMinMaxL0Excess = dvsReword rmmL0Excess
  , rangeMinMaxL1Min    = rmmL1Min
  , rangeMinMaxL1Max    = rmmL1Max
  , rangeMinMaxL1Excess = dvsReword rmmL1Excess
  , rangeMinMaxL2Min    = rmmL2Min
  , rangeMinMaxL2Max    = rmmL2Max
  , rangeMinMaxL2Excess = rmmL2Excess
  }
  where lenBP         = fromIntegral (length bp) :: Int
        lenL0         = lenBP
        lenL1         = (DVS.length rmmL0Min `div` pageSizeL1) + 1 :: Int
        lenL2         = (DVS.length rmmL0Min `div` pageSizeL2) + 1 :: Int
        allMinMaxL0   = dvConstructNI  lenL0 (\i -> if i == lenBP then (-64, -64, 0) else minMaxExcess1 (bp !!! fromIntegral i))
        allMinMaxL1   = dvConstructNI  lenL1 (\i -> minMaxExcess1 (dropTake (i * pageSizeL1) pageSizeL1 bp))
        allMinMaxL2   = dvConstructNI  lenL2 (\i -> minMaxExcess1 (dropTake (i * pageSizeL2) pageSizeL2 bp))
        rmmL0Excess   = dvsConstructNI lenL0 (\i -> fromIntegral (allExcess1 (pageFill i pageSizeL0 (-64) bp))) :: DVS.Vector Int16
        rmmL1Excess   = dvsConstructNI lenL1 (\i -> fromIntegral (allExcess1 (pageFill i pageSizeL1 (-64) bp))) :: DVS.Vector Int16
        rmmL2Excess   = dvsConstructNI lenL2 (\i -> fromIntegral (allExcess1 (pageFill i pageSizeL2 (-64) bp))) :: DVS.Vector Int16
        rmmL0Min      = dvsConstructNI lenL0 (\i -> let (minE, _, _) = allMinMaxL0 DV.! i in fromIntegral minE)
        rmmL1Min      = dvsConstructNI lenL1 (\i -> let (minE, _, _) = allMinMaxL1 DV.! i in fromIntegral minE)
        rmmL2Min      = dvsConstructNI lenL2 (\i -> let (minE, _, _) = allMinMaxL2 DV.! i in fromIntegral minE)
        rmmL0Max      = dvsConstructNI lenL0 (\i -> let (_, _, maxE) = allMinMaxL0 DV.! i in fromIntegral maxE)
        rmmL1Max      = dvsConstructNI lenL1 (\i -> let (_, _, maxE) = allMinMaxL1 DV.! i in fromIntegral maxE)
        rmmL2Max      = dvsConstructNI lenL2 (\i -> let (_, _, maxE) = allMinMaxL2 DV.! i in fromIntegral maxE)

dropTake :: DVS.Storable a => Int -> Int -> DVS.Vector a -> DVS.Vector a
dropTake n o = DVS.take o . DVS.drop n
{-# INLINE dropTake #-}

dvsReword :: (DVS.Storable a, Integral a, DVS.Storable b, Num b) => DVS.Vector a -> DVS.Vector b
dvsReword v = dvsConstructNI (DVS.length v) (\i -> fromIntegral (v DVS.! i))
{-# INLINE dvsReword #-}

pageFill :: DVS.Storable a => Int -> Int -> a -> DVS.Vector a -> DVS.Vector a
pageFill n s = dropTakeFill (n * s) s
{-# INLINE pageFill #-}

dropTakeFill :: DVS.Storable a => Int -> Int -> a -> DVS.Vector a -> DVS.Vector a
dropTakeFill n o a v =  let r = DVS.take o (DVS.drop n v) in
                        let len = DVS.length r in
                        if len == o then r else DVS.concat [r, DVS.fromList (replicate (o - len) a)]
{-# INLINE dropTakeFill #-}

dvConstructNI :: Int -> (Int -> a) -> DV.Vector a
dvConstructNI n g = DV.constructN n (g . DV.length)
{-# INLINE dvConstructNI #-}

dvsConstructNI :: DVS.Storable a => Int -> (Int -> a) -> DVS.Vector a
dvsConstructNI n g = DVS.constructN n (g . DVS.length)
{-# INLINE dvsConstructNI #-}

data FindState = FindBP
  | FindL0 | FindFromL0
  | FindL1 | FindFromL1
  | FindL2 | FindFromL2

rmm2FindClose  :: RangeMinMax -> Int -> Count -> FindState -> Maybe Count
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
      else  let excesses  = rangeMinMaxL0Excess v in
            let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
            rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + 64) FindFromL0
rmm2FindClose v s p FindL1 =
  let !i = p `div` (64 * pageSizeL1) in
  let !mins = rangeMinMaxL1Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindL0
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMinMaxL1Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL1)) FindFromL1
      else Nothing
rmm2FindClose v s p FindL2 =
  let !i = p `div` (64 * pageSizeL2) in
  let !mins = rangeMinMaxL2Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindL1
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMinMaxL2Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL2)) FindFromL2
      else Nothing
rmm2FindClose v s p FindFromL0
  | p `mod` 64 == 0             = rmm2FindClose v s p FindFromL1
  | 0 <= p && p < bitLength v   = rmm2FindClose v s p FindBP
  | otherwise                   = Nothing
rmm2FindClose v s p FindFromL1
  | p `mod` (64 * pageSizeL1) == 0  = if 0 <= p && p < bitLength v then rmm2FindClose v s p FindFromL2 else Nothing
  | 0 <= p && p < bitLength v       = rmm2FindClose v s p FindL0
  | otherwise                       = Nothing
rmm2FindClose v s p FindFromL2
  | p `mod` (64 * pageSizeL2) == 0  = if 0 <= p && p < bitLength v then rmm2FindClose v s p FindL2 else Nothing
  | 0 <= p && p < bitLength v       = rmm2FindClose v s p FindL1
  | otherwise                       = Nothing
{-# INLINE rmm2FindClose #-}

instance TestBit RangeMinMax where
  (.?.) = (.?.) . rangeMinMaxBP
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMax where
  rank1 = rank1 . rangeMinMaxBP
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMax where
  rank0 = rank0 . rangeMinMaxBP
  {-# INLINE rank0 #-}

instance BitLength RangeMinMax where
  bitLength = bitLength . rangeMinMaxBP
  {-# INLINE bitLength #-}

instance OpenAt RangeMinMax where
  openAt = openAt . rangeMinMaxBP
  {-# INLINE openAt #-}

instance CloseAt RangeMinMax where
  closeAt = closeAt . rangeMinMaxBP
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMax where
  newCloseAt = newCloseAt . rangeMinMaxBP
  {-# INLINE newCloseAt #-}

instance FindOpenN RangeMinMax where
  findOpenN = findOpenN . rangeMinMaxBP
  {-# INLINE findOpenN #-}

instance FindCloseN RangeMinMax where
  findCloseN v s p  = (+ 1) `fmap` rmm2FindClose v (fromIntegral s) (p - 1) FindFromL0
  {-# INLINE findCloseN  #-}

instance FindClose RangeMinMax where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindOpen RangeMinMax where
  findOpen = undefined
  {-# INLINE findOpen #-}

instance Enclose RangeMinMax where
  enclose = undefined
  {-# INLINE enclose #-}

instance BalancedParens RangeMinMax
