{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

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
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.NewCloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L0
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L1
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMaxL2 = RangeMinMaxL2
  { rangeMinMaxL2Base     :: !RangeMinMaxL1
  , rangeMinMaxL2Min      :: !(DVS.Vector Int16)
  , rangeMinMaxL2Max      :: !(DVS.Vector Int16)
  , rangeMinMaxL2Excess   :: !(DVS.Vector Int16)
  }

instance RangeMinMaxLevel RangeMinMaxL2 where
  rmmFactor _ = 32
  rmmBinWords v = rmmFactor v * rmmBinWords (rmmBase v)
  rmmBins = DVS.length . rangeMinMaxL2Min
  {-# INLINE rmmFactor    #-}
  {-# INLINE rmmBinWords  #-}
  {-# INLINE rmmBins      #-}

class MkRangeMinMaxL2 a where
  mkRangeMinMaxL2 :: a -> RangeMinMaxL2

instance MkRangeMinMaxL2 RangeMinMaxL1 where
  mkRangeMinMaxL2 rmmL1 = result
    where bp            = rangeMinMaxSimpleBP (rangeMinMaxSimple (rangeMinMaxL1Base rmmL1))
          lenL2         = (DVS.length (rangeMinMaxL1Min rmmL1) `div` rmmFactor result) + 1 :: Int
          allMinMaxL2   = DV.constructN lenL2 genMinMaxL2
          genMinMaxL2 v = let len = DV.length v in minMaxExcess1 (DVS.take (rmmBinWords result) (DVS.drop (len * rmmBinWords result) bp))
          rangeMinMaxL2ExcessA = DVS.constructN lenL2 (\v -> let (_, e,    _) = allMinMaxL2 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int16
          result = RangeMinMaxL2
            { rangeMinMaxL2Base     = rmmL1
            , rangeMinMaxL2Min      = DVS.constructN lenL2 (\v -> let (minE, _, _) = allMinMaxL2 DV.! DVS.length v in fromIntegral minE)
            , rangeMinMaxL2Max      = DVS.constructN lenL2 (\v -> let (_, _, maxE) = allMinMaxL2 DV.! DVS.length v in fromIntegral maxE)
            , rangeMinMaxL2Excess   = rangeMinMaxL2ExcessA
            }

instance MkRangeMinMaxL2 RangeMinMaxSimple where
  mkRangeMinMaxL2 = mkRangeMinMaxL2 . mkRangeMinMaxL1

instance MkRangeMinMaxL2 (DVS.Vector Word64) where
  mkRangeMinMaxL2 = mkRangeMinMaxL2 . mkRangeMinMaxSimple
  {-# INLINE mkRangeMinMaxL2 #-}

instance TestBit RangeMinMaxL2 where
  (.?.) = (.?.) . rangeMinMaxL2Base
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMaxL2 where
  rank1 = rank1 . rangeMinMaxL2Base
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMaxL2 where
  rank0 = rank0 . rangeMinMaxL2Base
  {-# INLINE rank0 #-}

instance BitLength RangeMinMaxL2 where
  bitLength = bitLength . rangeMinMaxL2Base
  {-# INLINE bitLength #-}

instance RangeMinMaxDerived RangeMinMaxL2 where
  type RangeMinMaxBase RangeMinMaxL2 = RangeMinMaxL1
  rmmBase = rangeMinMaxL2Base
  {-# INLINE rmmBase #-}

instance RangeMinMax RangeMinMaxL2 where
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
    where mins                  = rangeMinMaxL2Min v
          excesses              = rangeMinMaxL2Excess v
  {-# INLINE rmmFindCloseDispatch #-}
  {-# INLINE rmmFindCloseN        #-}

instance OpenAt RangeMinMaxL2 where
  openAt = openAt . rangeMinMaxL2Base
  {-# INLINE openAt #-}

instance CloseAt RangeMinMaxL2 where
  closeAt = closeAt . rangeMinMaxL2Base
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMaxL2 where
  newCloseAt = newCloseAt . rangeMinMaxL2Base
  {-# INLINE newCloseAt #-}

instance BalancedParens RangeMinMaxL2 where
  -- findOpenN         = findOpenN   . rangeMinMaxBP
  findCloseN v s p = (+ 1) `fmap` rmmFindClose v (fromIntegral s) (p - 1)

  -- {-# INLINE findOpenN   #-}
  {-# INLINE findCloseN  #-}
