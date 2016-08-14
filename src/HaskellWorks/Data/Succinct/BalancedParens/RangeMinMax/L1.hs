{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

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
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L0
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMaxL1 = RangeMinMaxL1
  { rangeMinMaxL1Simple :: RangeMinMaxSimple
  , rangeMinMaxL1L0Min    :: DVS.Vector Int8
  , rangeMinMaxL1L0Max    :: DVS.Vector Int8
  , rangeMinMaxL1L0Excess :: DVS.Vector Int8
  , rangeMinMaxL1Min    :: DVS.Vector Int16
  , rangeMinMaxL1Max    :: DVS.Vector Int16
  , rangeMinMaxL1Excess :: DVS.Vector Int16
  }

wordsL1 :: Int
wordsL1 = 8
{-# INLINE wordsL1 #-}

bitsL1 :: Count
bitsL1 = fromIntegral (wordsL1 * 64)

class MkRangeMinMaxL1 a where
  mkRangeMinMaxL1 :: a -> RangeMinMaxL1

instance MkRangeMinMaxL1 RangeMinMaxSimple where
  mkRangeMinMaxL1 simple = RangeMinMaxL1
    { rangeMinMaxL1Simple = simple
    , rangeMinMaxL1L0Min    = DVS.constructN lenL0 (\v -> let (minE, _, _) = allMinMaxL0 DV.! DVS.length v in fromIntegral minE)
    , rangeMinMaxL1L0Max    = DVS.constructN lenL0 (\v -> let (_, _, maxE) = allMinMaxL0 DV.! DVS.length v in fromIntegral maxE)
    , rangeMinMaxL1L0Excess = rangeMinMaxL0ExcessA
    , rangeMinMaxL1Min      = DVS.constructN lenL1 (\v -> let (minE, _, _) = allMinMaxL1 DV.! DVS.length v in fromIntegral minE)
    , rangeMinMaxL1Max      = DVS.constructN lenL1 (\v -> let (_, _, maxE) = allMinMaxL1 DV.! DVS.length v in fromIntegral maxE)
    , rangeMinMaxL1Excess   = rangeMinMaxL1ExcessA
    }
    where lenSimple     = fromIntegral (vLength (rangeMinMaxSimpleBP simple)) :: Int
          lenL0         = lenSimple + 1
          bp            = rangeMinMaxSimpleBP simple
          allMinMaxL0   = DV.constructN lenL0 genMinMaxL0
          genMinMaxL0 v = let doneLen = DV.length v in if doneLen == lenSimple
                            then (0, 0, 0)
                            else minMaxExcess1 (bp !!! fromIntegral doneLen)
          lenL1         = fromIntegral (vLength bp) `div` wordsL1 + 1 :: Int
          allMinMaxL1   = DV.constructN lenL1 genMinMaxL1
          genMinMaxL1 v = let len = DV.length v in
                          minMaxExcess1 (DVS.take wordsL1 (DVS.drop (fromIntegral len * wordsL1) bp))
          rangeMinMaxL0ExcessA = DVS.constructN lenL0 (\v -> let (_, e,    _) = allMinMaxL0 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int8
          rangeMinMaxL1ExcessA = DVS.constructN lenL1 (\v -> let (_, e,    _) = allMinMaxL1 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int16

instance MkRangeMinMaxL1 (DVS.Vector Word64) where
  mkRangeMinMaxL1 = mkRangeMinMaxL1 . mkRangeMinMaxSimple

instance TestBit RangeMinMaxL1 where
  (.?.) = (.?.) . rangeMinMaxL1Simple
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMaxL1 where
  rank1 = rank1 . rangeMinMaxL1Simple
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMaxL1 where
  rank0 = rank0 . rangeMinMaxL1Simple
  {-# INLINE rank0 #-}

instance BitLength RangeMinMaxL1 where
  bitLength = bitLength . rangeMinMaxL1Simple
  {-# INLINE bitLength #-}

instance RangeMinMaxDerived RangeMinMaxL1 where
  type RangeMinMaxBase RangeMinMaxL1 = RangeMinMaxL0
  rmmBase v = RangeMinMaxL0
    { rangeMinMaxSimple   = rangeMinMaxL1Simple v
    , rangeMinMaxL0Min    = rangeMinMaxL1L0Min v
    , rangeMinMaxL0Max    = rangeMinMaxL1L0Max v
    , rangeMinMaxL0Excess = rangeMinMaxL1L0Excess v
    }
  {-# INLINE rmmBase #-}

instance RangeMinMax RangeMinMaxL1 where
  rmmFindCloseDispatch v s p = if (p - 1) `mod` bitsL1 == 0
    then rmmFindCloseN v s p
    else rmmFindCloseDispatch (rmmBase v) s p
  rmmFindCloseN v s p =
    let i = (p - 1) `div` bitsL1 in
    let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
    if fromIntegral s + minE <= 0
      then  rmmFindCloseN (rmmBase v) s p
      else if v `closeAt` p && s <= 1
        then Progress p
        else let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmmFindClose  v (fromIntegral (excess + fromIntegral s)) (p + bitsL1)
    where mins                  = rangeMinMaxL1Min v
          excesses              = rangeMinMaxL1Excess v
  {-# INLINE rmmFindCloseDispatch #-}
  {-# INLINE rmmFindCloseN        #-}

instance OpenAt RangeMinMaxL1 where
  openAt = openAt . rangeMinMaxL1Simple
  {-# INLINE openAt #-}

instance CloseAt RangeMinMaxL1 where
  closeAt = closeAt . rangeMinMaxL1Simple
  {-# INLINE closeAt #-}

instance BalancedParens RangeMinMaxL1 where
  -- findOpenN         = findOpenN   . rangeMinMaxBP
  findCloseN v s p  = resultToMaybe (rmmFindClose v (fromIntegral s) p)

  -- {-# INLINE findOpenN   #-}
  {-# INLINE findCloseN  #-}
