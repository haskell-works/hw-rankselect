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
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.NewCloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L0
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMaxL1 = RangeMinMaxL1
  { rangeMinMaxL1Base     :: !RangeMinMaxL0
  , rangeMinMaxL1Min      :: !(DVS.Vector Int16)
  , rangeMinMaxL1Max      :: !(DVS.Vector Int16)
  , rangeMinMaxL1Excess   :: !(DVS.Vector Int16)
  }

instance RangeMinMaxLevel RangeMinMaxL1 where
  rmmFactor _ = 32
  rmmBinWords v = rmmFactor v * rmmBinWords (rmmBase v)
  rmmBins = DVS.length . rangeMinMaxL1Min
  {-# INLINE rmmFactor    #-}
  {-# INLINE rmmBinWords  #-}
  {-# INLINE rmmBins      #-}

class MkRangeMinMaxL1 a where
  mkRangeMinMaxL1 :: a -> RangeMinMaxL1

instance MkRangeMinMaxL1 RangeMinMaxL0 where
  mkRangeMinMaxL1 rmmL0 = result
    where bp            = rangeMinMaxSimpleBP (rangeMinMaxSimple rmmL0)
          lenL1         = (DVS.length (rangeMinMaxL0Min rmmL0) `div` rmmFactor result) + 1 :: Int
          allMinMaxL1   = DV.constructN lenL1 genMinMaxL1
          genMinMaxL1 v = let len = DV.length v in minMaxExcess1 (DVS.take (rmmBinWords result) (DVS.drop (len * rmmBinWords result) bp))
          rangeMinMaxL1ExcessA = DVS.constructN lenL1 (\v -> let (_, e,    _) = allMinMaxL1 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int16
          result        = RangeMinMaxL1
            { rangeMinMaxL1Base     = rmmL0
            , rangeMinMaxL1Min      = DVS.constructN lenL1 (\v -> let (minE, _, _) = allMinMaxL1 DV.! DVS.length v in fromIntegral minE)
            , rangeMinMaxL1Max      = DVS.constructN lenL1 (\v -> let (_, _, maxE) = allMinMaxL1 DV.! DVS.length v in fromIntegral maxE)
            , rangeMinMaxL1Excess   = rangeMinMaxL1ExcessA
            }

instance MkRangeMinMaxL1 RangeMinMaxSimple where
  mkRangeMinMaxL1 = mkRangeMinMaxL1 . mkRangeMinMaxL0

instance MkRangeMinMaxL1 (DVS.Vector Word64) where
  mkRangeMinMaxL1 = mkRangeMinMaxL1 . mkRangeMinMaxSimple
  {-# INLINE mkRangeMinMaxL1 #-}

instance TestBit RangeMinMaxL1 where
  (.?.) = (.?.) . rangeMinMaxL1Base
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMaxL1 where
  rank1 = rank1 . rangeMinMaxL1Base
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMaxL1 where
  rank0 = rank0 . rangeMinMaxL1Base
  {-# INLINE rank0 #-}

instance BitLength RangeMinMaxL1 where
  bitLength = bitLength . rangeMinMaxL1Base
  {-# INLINE bitLength #-}

instance RangeMinMaxDerived RangeMinMaxL1 where
  type RangeMinMaxBase RangeMinMaxL1 = RangeMinMaxL0
  rmmBase = rangeMinMaxL1Base
  {-# INLINE rmmBase #-}

instance RangeMinMax RangeMinMaxL1 where
  rmmFindCloseDispatch v s p = if (p - 1) `mod` rmmBinBits v == 0
    then rmmFindCloseN v s p
    else rmmFindCloseDispatch (rmmBase v) s p
  rmmFindCloseN v s p =
    let q = p - 1 in
    let i = q `div` rmmBinBits v in
    let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
    if fromIntegral s + minE <= 0
      then  rmmFindCloseN (rmmBase v) s p
      else if v `newCloseAt` q && s <= 1
        then Just p
        else let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmmFindClose  v (fromIntegral (excess + fromIntegral s)) (p + rmmBinBits v)
    where mins                  = rangeMinMaxL1Min v
          excesses              = rangeMinMaxL1Excess v
  {-# INLINE rmmFindCloseDispatch #-}
  {-# INLINE rmmFindCloseN        #-}

instance OpenAt RangeMinMaxL1 where
  openAt = openAt . rangeMinMaxL1Base
  {-# INLINE openAt #-}

instance CloseAt RangeMinMaxL1 where
  closeAt = closeAt . rangeMinMaxL1Base
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMaxL1 where
  newCloseAt = newCloseAt . rangeMinMaxL1Base
  {-# INLINE newCloseAt #-}

instance BalancedParens RangeMinMaxL1 where
  -- findOpenN         = findOpenN   . rangeMinMaxBP
  findCloseN v s = rmmFindClose v (fromIntegral s)

  -- {-# INLINE findOpenN   #-}
  {-# INLINE findCloseN  #-}
