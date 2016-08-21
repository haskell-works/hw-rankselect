{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.L1
  ( RangeMinMax2L1(..)
  , mkRangeMinMax2L1
  ) where

import           Data.Int
import qualified Data.Vector                                              as DV
import qualified Data.Vector.Storable                                     as DVS
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
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.L0
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMax2L1 = RangeMinMax2L1
  { rangeMinMaxL1Base     :: !RangeMinMax2L0
  , rangeMinMaxL1Min      :: !(DVS.Vector Int16)
  , rangeMinMaxL1Max      :: !(DVS.Vector Int16)
  , rangeMinMaxL1Excess   :: !(DVS.Vector Int16)
  }

instance RangeMinMax2Level RangeMinMax2L1 where
  rmmFactor _ = 32
  rmmBinWords v = rmmFactor v * rmmBinWords (rmmBase v)
  rmmBins = DVS.length . rangeMinMaxL1Min
  {-# INLINE rmmFactor    #-}
  {-# INLINE rmmBinWords  #-}
  {-# INLINE rmmBins      #-}

class MkRangeMinMax2L1 a where
  mkRangeMinMax2L1 :: a -> RangeMinMax2L1

instance MkRangeMinMax2L1 RangeMinMax2L0 where
  mkRangeMinMax2L1 rmmL0 = result
    where bp            = rangeMinMaxSimpleBP (rangeMinMaxSimple rmmL0)
          lenL1         = (DVS.length (rangeMinMaxL0Min rmmL0) `div` rmmFactor result) + 1 :: Int
          allMinMaxL1   = DV.constructN lenL1 genMinMaxL1
          genMinMaxL1 v = let len = DV.length v in minMaxExcess1 (DVS.take (rmmBinWords result) (DVS.drop (len * rmmBinWords result) bp))
          rangeMinMaxL1ExcessA = DVS.constructN lenL1 (\v -> let (_, e,    _) = allMinMaxL1 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int16
          result        = RangeMinMax2L1
            { rangeMinMaxL1Base     = rmmL0
            , rangeMinMaxL1Min      = DVS.constructN lenL1 (\v -> let (minE, _, _) = allMinMaxL1 DV.! DVS.length v in fromIntegral minE)
            , rangeMinMaxL1Max      = DVS.constructN lenL1 (\v -> let (_, _, maxE) = allMinMaxL1 DV.! DVS.length v in fromIntegral maxE)
            , rangeMinMaxL1Excess   = rangeMinMaxL1ExcessA
            }
  {-# INLINE mkRangeMinMax2L1 #-}

instance MkRangeMinMax2L1 RangeMinMax2Simple where
  mkRangeMinMax2L1 = mkRangeMinMax2L1 . mkRangeMinMax2L0
  {-# INLINE mkRangeMinMax2L1 #-}

instance MkRangeMinMax2L1 (DVS.Vector Word64) where
  mkRangeMinMax2L1 = mkRangeMinMax2L1 . mkRangeMinMax2Simple
  {-# INLINE mkRangeMinMax2L1 #-}

instance TestBit RangeMinMax2L1 where
  (.?.) = (.?.) . rangeMinMaxL1Base
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMax2L1 where
  rank1 = rank1 . rangeMinMaxL1Base
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMax2L1 where
  rank0 = rank0 . rangeMinMaxL1Base
  {-# INLINE rank0 #-}

instance BitLength RangeMinMax2L1 where
  bitLength = bitLength . rangeMinMaxL1Base
  {-# INLINE bitLength #-}

instance RangeMinMax2Derived RangeMinMax2L1 where
  type RangeMinMax2Base RangeMinMax2L1 = RangeMinMax2L0
  rmmBase = rangeMinMaxL1Base
  {-# INLINE rmmBase #-}

instance RangeMinMax2 RangeMinMax2L1 where
  rmmFindCloseDispatch v s p = if p `mod` rmmBinBits v == 0
    then rmmFindCloseN v s p
    else rmmFindCloseDispatch (rmmBase v) s p
  rmmFindCloseN v s p =
    let i = p `div` rmmBinBits v in
    let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
    if fromIntegral s + minE <= 0
      then  rmmFindCloseN (rmmBase v) s p
      else if v `newCloseAt` p && s <= 1
        then Just p
        else let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmmFindClose  v (fromIntegral (excess + fromIntegral s)) (p + rmmBinBits v)
    where mins                  = rangeMinMaxL1Min v
          excesses              = rangeMinMaxL1Excess v
  {-# INLINE rmmFindCloseDispatch #-}
  {-# INLINE rmmFindCloseN        #-}

instance OpenAt RangeMinMax2L1 where
  openAt = openAt . rangeMinMaxL1Base
  {-# INLINE openAt #-}

instance CloseAt RangeMinMax2L1 where
  closeAt = closeAt . rangeMinMaxL1Base
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMax2L1 where
  newCloseAt = newCloseAt . rangeMinMaxL1Base
  {-# INLINE newCloseAt #-}

instance FindOpenN RangeMinMax2L1 where
  findOpenN = findOpenN . rangeMinMaxL1Base
  {-# INLINE findOpenN #-}

instance FindCloseN RangeMinMax2L1 where
  findCloseN v s p = (+ 1) `fmap` rmmFindClose v (fromIntegral s) (p - 1)
  {-# INLINE findCloseN #-}

instance FindOpen RangeMinMax2L1 where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose RangeMinMax2L1 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose RangeMinMax2L1 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens RangeMinMax2L1
