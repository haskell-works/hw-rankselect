{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.L0
  ( RangeMinMax2L0(..)
  , mkRangeMinMax2L0
  ) where

import           Data.Int
import qualified Data.Vector                                                  as DV
import qualified Data.Vector.Storable                                         as DVS
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
import           HaskellWorks.Data.Succinct.BalancedParens.NewCloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMax2L0 = RangeMinMax2L0
  { rangeMinMaxSimple   :: !RangeMinMax2Simple
  , rangeMinMaxL0Min    :: !(DVS.Vector Int8)
  , rangeMinMaxL0Max    :: !(DVS.Vector Int8)
  , rangeMinMaxL0Excess :: !(DVS.Vector Int8)
  }

instance RangeMinMax2Level RangeMinMax2L0 where
  rmmFactor _ = 1
  rmmBinWords _ = 1
  rmmBins = DVS.length . rangeMinMaxL0Min
  {-# INLINE rmmFactor #-}
  {-# INLINE rmmBinWords #-}
  {-# INLINE rmmBins #-}

class MkRangeMinMax2L0 a where
  mkRangeMinMax2L0 :: a -> RangeMinMax2L0

instance MkRangeMinMax2L0 RangeMinMax2Simple where
  mkRangeMinMax2L0 simple = RangeMinMax2L0
    { rangeMinMaxSimple   = simple
    , rangeMinMaxL0Min    = DVS.constructN (len0 + 1) (\v -> let (minE, _, _) = allMinMax DV.! DVS.length v in fromIntegral minE)
    , rangeMinMaxL0Max    = DVS.constructN (len0 + 1) (\v -> let (_, _, maxE) = allMinMax DV.! DVS.length v in fromIntegral maxE)
    , rangeMinMaxL0Excess = DVS.constructN (len0 + 1) (\v -> let (_, e,    _) = allMinMax DV.! DVS.length v in fromIntegral e)
    }
    where len0        = fromIntegral (vLength (rangeMinMaxSimpleBP simple)) :: Int
          allMinMax   = DV.constructN (len0 + 1) genMinMax
          genMinMax v = let len = DV.length v in
                        if len == len0
                          then (0, 0, 0)
                          else minMaxExcess1 (rangeMinMaxSimpleBP simple !!! fromIntegral len)
  {-# INLINE mkRangeMinMax2L0 #-}

instance MkRangeMinMax2L0 (DVS.Vector Word64) where
  mkRangeMinMax2L0 = mkRangeMinMax2L0 . mkRangeMinMax2Simple
  {-# INLINE mkRangeMinMax2L0 #-}

instance RangeMinMax2Derived RangeMinMax2L0 where
  type RangeMinMax2Base RangeMinMax2L0 = RangeMinMax2Simple
  rmmBase = rangeMinMaxSimple
  {-# INLINE rmmBase #-}

instance TestBit RangeMinMax2L0 where
  (.?.) = (.?.) . rangeMinMaxSimple
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMax2L0 where
  rank1 = rank1 . rangeMinMaxSimple
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMax2L0 where
  rank0 = rank0 . rangeMinMaxSimple
  {-# INLINE rank0 #-}

instance BitLength RangeMinMax2L0 where
  bitLength = bitLength . rangeMinMaxSimple
  {-# INLINE bitLength #-}

instance OpenAt RangeMinMax2L0 where
  openAt = openAt . rangeMinMaxSimple
  {-# INLINE openAt #-}

instance CloseAt RangeMinMax2L0 where
  closeAt = closeAt . rangeMinMaxSimple
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMax2L0 where
  newCloseAt = newCloseAt . rangeMinMaxSimple
  {-# INLINE newCloseAt #-}

instance RangeMinMax2 RangeMinMax2L0 where
  rmmFindCloseDispatch v s p = if p `mod` 64 == 0
    then rmmFindCloseN v s p
    else rmmFindCloseDispatch (rangeMinMaxSimple v) s p
  rmmFindCloseN v s p =
    let i = p `div` 64 in
    let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
    if fromIntegral s + minE <= 0
      then  rmmFindCloseN (rangeMinMaxSimple v) s p
      else if not (v `newCloseAt` p && s <= 1)
        then let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmmFindCloseN v (fromIntegral (excess + fromIntegral s)) (p + 64)
        else Just p
    where mins                  = rangeMinMaxL0Min v
          excesses              = rangeMinMaxL0Excess v
  {-# INLINE rmmFindCloseDispatch #-}
  {-# INLINE rmmFindCloseN        #-}

instance FindOpenN RangeMinMax2L0 where
  findOpenN = findOpenN . rangeMinMaxSimple
  {-# INLINE findOpenN #-}

instance FindCloseN RangeMinMax2L0 where
  findCloseN v s p = (+ 1) `fmap` rmmFindClose v (fromIntegral s) (p - 1)
  {-# INLINE findCloseN #-}

instance FindOpen RangeMinMax2L0 where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose RangeMinMax2L0 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose RangeMinMax2L0 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens RangeMinMax2L0
