{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2
  ( RangeMinMax2(..)
  , mkRangeMinMax2
  , genMin
  , genMax
  ) where

import           Data.Int
import qualified Data.Vector                                                    as DV
import qualified Data.Vector.Storable                                           as DVS
-- import           Data.Word
import           HaskellWorks.Data.AtIndex
import           HaskellWorks.Data.Bits.AllExcess.AllExcess1
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Excess.MinMaxExcess1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.RankSelect.Base.Rank0
import           HaskellWorks.Data.RankSelect.Base.Rank1
import           HaskellWorks.Data.Succinct.BalancedParens.BalancedParens
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.Enclose
import           HaskellWorks.Data.Succinct.BalancedParens.FindClose
import           HaskellWorks.Data.Succinct.BalancedParens.FindCloseN
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpen
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpenN
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           HaskellWorks.Data.Succinct.BalancedParens.NewCloseAt
import           HaskellWorks.Data.Vector.AsVector64
import           Prelude hiding (length)

data RangeMinMax2 a = RangeMinMax2
  { rangeMinMax2BP       :: !a
  , rangeMinMax2L0Min    :: !(DVS.Vector Int8)
  , rangeMinMax2L0Max    :: !(DVS.Vector Int8)
  , rangeMinMax2L0Excess :: !(DVS.Vector Int8)
  , rangeMinMax2L1Min    :: !(DVS.Vector Int16)
  , rangeMinMax2L1Max    :: !(DVS.Vector Int16)
  , rangeMinMax2L1Excess :: !(DVS.Vector Int16)
  , rangeMinMax2L2Min    :: !(DVS.Vector Int16)
  , rangeMinMax2L2Max    :: !(DVS.Vector Int16)
  , rangeMinMax2L2Excess :: !(DVS.Vector Int16)
  , rangeMinMax2L3Min    :: !(DVS.Vector Int16)
  , rangeMinMax2L3Max    :: !(DVS.Vector Int16)
  , rangeMinMax2L3Excess :: !(DVS.Vector Int16)
  , rangeMinMax2L4Min    :: !(DVS.Vector Int16)
  , rangeMinMax2L4Max    :: !(DVS.Vector Int16)
  , rangeMinMax2L4Excess :: !(DVS.Vector Int16)
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

factorL3 :: Integral a => a
factorL3 = 32
{-# INLINE factorL3 #-}

factorL4 :: Integral a => a
factorL4 = 32
{-# INLINE factorL4 #-}

pageSizeL0 :: Integral a => a
pageSizeL0 = factorL0
{-# INLINE pageSizeL0 #-}

pageSizeL1 :: Integral a => a
pageSizeL1 = pageSizeL0 * factorL1
{-# INLINE pageSizeL1 #-}

pageSizeL2 :: Integral a => a
pageSizeL2 = pageSizeL1 * factorL2
{-# INLINE pageSizeL2 #-}

pageSizeL3 :: Integral a => a
pageSizeL3 = pageSizeL2 * factorL3
{-# INLINE pageSizeL3 #-}

pageSizeL4 :: Integral a => a
pageSizeL4 = pageSizeL3 * factorL4
{-# INLINE pageSizeL4 #-}

mkRangeMinMax2 :: AsVector64 a => a -> RangeMinMax2 a
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
  , rangeMinMax2L3Min    = rmmL3Min
  , rangeMinMax2L3Max    = rmmL3Max
  , rangeMinMax2L3Excess = rmmL3Excess
  , rangeMinMax2L4Min    = rmmL4Min
  , rangeMinMax2L4Max    = rmmL4Max
  , rangeMinMax2L4Excess = rmmL4Excess
  }
  where bpv           = asVector64 bp
        lenBP         = fromIntegral (length bpv) :: Int
        lenL0         = lenBP
        lenL1         = (DVS.length rmmL0Min `div` pageSizeL1) + 1 :: Int
        lenL2         = (DVS.length rmmL0Min `div` pageSizeL2) + 1 :: Int
        lenL3         = (DVS.length rmmL0Min `div` pageSizeL3) + 1 :: Int
        lenL4         = (DVS.length rmmL0Min `div` pageSizeL4) + 1 :: Int
        allMinMaxL0   = dvConstructNI  lenL0 (\i -> if i == lenBP then (-64, -64, 0) else minMaxExcess1 (bpv !!! fromIntegral i))
        rmmL0Excess   = dvsConstructNI lenL0 (\i -> fromIntegral (allExcess1 (pageFill i pageSizeL0 (-64) bpv))) :: DVS.Vector Int16
        rmmL1Excess   = dvsConstructNI lenL1 (\i -> fromIntegral (allExcess1 (pageFill i pageSizeL1 (-64) bpv))) :: DVS.Vector Int16
        rmmL2Excess   = dvsConstructNI lenL2 (\i -> fromIntegral (allExcess1 (pageFill i pageSizeL2 (-64) bpv))) :: DVS.Vector Int16
        rmmL3Excess   = dvsConstructNI lenL3 (\i -> fromIntegral (allExcess1 (pageFill i pageSizeL3 (-64) bpv))) :: DVS.Vector Int16
        rmmL4Excess   = dvsConstructNI lenL4 (\i -> fromIntegral (allExcess1 (pageFill i pageSizeL4 (-64) bpv))) :: DVS.Vector Int16
        rmmL0Min      = dvsConstructNI lenL0 (\i -> let (minE, _, _) = allMinMaxL0 DV.! i in fromIntegral minE) :: DVS.Vector Int16
        rmmL1Min      = dvsConstructNI lenL1 (\i -> genMin 0 (pageFill i factorL1 0 rmmL0Min) (pageFill i factorL1 0 rmmL0Excess))
        rmmL2Min      = dvsConstructNI lenL2 (\i -> genMin 0 (pageFill i factorL2 0 rmmL1Min) (pageFill i factorL2 0 rmmL1Excess))
        rmmL3Min      = dvsConstructNI lenL3 (\i -> genMin 0 (pageFill i factorL3 0 rmmL2Min) (pageFill i factorL3 0 rmmL2Excess))
        rmmL4Min      = dvsConstructNI lenL4 (\i -> genMin 0 (pageFill i factorL4 0 rmmL3Min) (pageFill i factorL4 0 rmmL3Excess))
        rmmL0Max      = dvsConstructNI lenL0 (\i -> let (_, _, maxE) = allMinMaxL0 DV.! i in fromIntegral maxE) :: DVS.Vector Int16
        rmmL1Max      = dvsConstructNI lenL1 (\i -> genMax 0 (pageFill i factorL1 0 rmmL0Max) (pageFill i factorL1 0 rmmL0Excess))
        rmmL2Max      = dvsConstructNI lenL2 (\i -> genMax 0 (pageFill i factorL2 0 rmmL1Max) (pageFill i factorL2 0 rmmL1Excess))
        rmmL3Max      = dvsConstructNI lenL3 (\i -> genMax 0 (pageFill i factorL3 0 rmmL2Max) (pageFill i factorL3 0 rmmL2Excess))
        rmmL4Max      = dvsConstructNI lenL4 (\i -> genMax 0 (pageFill i factorL4 0 rmmL3Max) (pageFill i factorL4 0 rmmL3Excess))

genMin :: (Integral a, DVS.Storable a) => a -> DVS.Vector a -> DVS.Vector a -> a
genMin mL mins excesses = if not (DVS.null mins) || not (DVS.null excesses)
  then genMin (dvsLastOrZero mins `min` (mL + dvsLastOrZero excesses)) (DVS.init mins) (DVS.init excesses)
  else mL

genMax :: (Integral a, DVS.Storable a) => a -> DVS.Vector a -> DVS.Vector a -> a
genMax mL maxs excesses = if not (DVS.null maxs) || not (DVS.null excesses)
  then genMax (dvsLastOrZero maxs `max` (mL + dvsLastOrZero excesses)) (DVS.init maxs) (DVS.init excesses)
  else mL

pageFill :: DVS.Storable a => Int -> Int -> a -> DVS.Vector a -> DVS.Vector a
pageFill n s = dropTakeFill (n * s) s
{-# INLINE pageFill #-}

dropTakeFill :: DVS.Storable a => Int -> Int -> a -> DVS.Vector a -> DVS.Vector a
dropTakeFill n s a v =  let r = DVS.take s (DVS.drop n v) in
                        let rLen = DVS.length r in
                        if rLen == s then r else DVS.concat [r, DVS.replicate (s - rLen) a]
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

dvsLastOrZero :: (DVS.Storable a, Integral a) => DVS.Vector a -> a
dvsLastOrZero v = if not (DVS.null v) then DVS.last v else 0
{-# INLINE dvsLastOrZero #-}

data FindState = FindBP
  | FindL0 | FindFromL0
  | FindL1 | FindFromL1
  | FindL2 | FindFromL2
  | FindL3 | FindFromL3
  | FindL4 | FindFromL4

rmm2FindClose  :: (BitLength a, NewCloseAt a) => RangeMinMax2 a -> Int -> Count -> FindState -> Maybe Count
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
      else  let excesses  = rangeMinMax2L0Excess v in
            let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
            rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + 64) FindFromL0
rmm2FindClose v s p FindL1 =
  let !i = p `div` (64 * pageSizeL1) in
  let !mins = rangeMinMax2L1Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindL0
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMinMax2L1Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL1)) FindFromL1
      else Nothing
rmm2FindClose v s p FindL2 =
  let !i = p `div` (64 * pageSizeL2) in
  let !mins = rangeMinMax2L2Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindL1
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMinMax2L2Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL2)) FindFromL2
      else Nothing
rmm2FindClose v s p FindL3 =
  let !i = p `div` (64 * pageSizeL3) in
  let !mins = rangeMinMax2L3Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindL2
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMinMax2L3Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL3)) FindFromL3
        else Nothing
rmm2FindClose v s p FindL4 =
  let !i = p `div` (64 * pageSizeL4) in
  let !mins = rangeMinMax2L4Min v in
  let !minE = fromIntegral (mins !!! fromIntegral i) :: Int in
  if fromIntegral s + minE <= 0
    then rmm2FindClose v s p FindL3
    else if 0 <= p && p < bitLength v
      then if v `newCloseAt` p && s <= 1
        then Just p
        else  let excesses  = rangeMinMax2L4Excess v in
              let excess    = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmm2FindClose v (fromIntegral (excess + fromIntegral s)) (p + (64 * pageSizeL4)) FindFromL4
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
  | p `mod` (64 * pageSizeL2) == 0  = if 0 <= p && p < bitLength v then rmm2FindClose v s p FindFromL3 else Nothing
  | 0 <= p && p < bitLength v       = rmm2FindClose v s p FindL1
  | otherwise                       = Nothing
rmm2FindClose v s p FindFromL3
  | p `mod` (64 * pageSizeL3) == 0  = if 0 <= p && p < bitLength v then rmm2FindClose v s p FindFromL4 else Nothing
  | 0 <= p && p < bitLength v       = rmm2FindClose v s p FindL2
  | otherwise                       = Nothing
rmm2FindClose v s p FindFromL4
  | p `mod` (64 * pageSizeL4) == 0  = if 0 <= p && p < bitLength v then rmm2FindClose v s p FindL4 else Nothing
  | 0 <= p && p < bitLength v       = rmm2FindClose v s p FindL3
  | otherwise                       = Nothing
{-# INLINE rmm2FindClose #-}

instance TestBit a => TestBit (RangeMinMax2 a) where
  (.?.) = (.?.) . rangeMinMax2BP
  {-# INLINE (.?.) #-}

instance Rank1 a => Rank1 (RangeMinMax2 a) where
  rank1 = rank1 . rangeMinMax2BP
  {-# INLINE rank1 #-}

instance Rank0 a => Rank0 (RangeMinMax2 a) where
  rank0 = rank0 . rangeMinMax2BP
  {-# INLINE rank0 #-}

instance BitLength a => BitLength (RangeMinMax2 a) where
  bitLength = bitLength . rangeMinMax2BP
  {-# INLINE bitLength #-}

instance OpenAt a => OpenAt (RangeMinMax2 a) where
  openAt = openAt . rangeMinMax2BP
  {-# INLINE openAt #-}

instance CloseAt a => CloseAt (RangeMinMax2 a) where
  closeAt = closeAt . rangeMinMax2BP
  {-# INLINE closeAt #-}

instance NewCloseAt a => NewCloseAt (RangeMinMax2 a) where
  newCloseAt = newCloseAt . rangeMinMax2BP
  {-# INLINE newCloseAt #-}

instance FindOpenN a => FindOpenN (RangeMinMax2 a) where
  findOpenN = findOpenN . rangeMinMax2BP
  {-# INLINE findOpenN #-}

instance (BitLength a, FindCloseN a, NewCloseAt a) => FindCloseN (RangeMinMax2 a) where
  findCloseN v s p  = (+ 1) `fmap` rmm2FindClose v (fromIntegral s) (p - 1) FindFromL0
  {-# INLINE findCloseN  #-}

instance (BitLength a, NewCloseAt a, CloseAt a, FindCloseN a) => FindClose (RangeMinMax2 a) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v 1 (p + 1)
  {-# INLINE findClose #-}

instance FindOpen (RangeMinMax2 a) where
  findOpen = undefined
  {-# INLINE findOpen #-}

instance Enclose (RangeMinMax2 a) where
  enclose = undefined
  {-# INLINE enclose #-}

instance (BitLength a, NewCloseAt a, CloseAt a, OpenAt a, FindCloseN a) => BalancedParens (RangeMinMax2 a)
