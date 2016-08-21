{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L3
  ( RangeMinMaxL3(..)
  , mkRangeMinMaxL3
  ) where

import           Data.Int
import qualified Data.Vector                                              as DV
import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.Enclose
import           HaskellWorks.Data.Succinct.BalancedParens.FindClose
import           HaskellWorks.Data.Succinct.BalancedParens.FindCloseN
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpen
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpenN
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.NewCloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L0
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L1
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L2
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMaxL3 = RangeMinMaxL3
  { rangeMinMaxL3Base     :: !RangeMinMaxL2
  , rangeMinMaxL3Min      :: !(DVS.Vector Int16)
  , rangeMinMaxL3Max      :: !(DVS.Vector Int16)
  , rangeMinMaxL3Excess   :: !(DVS.Vector Int16)
  }

instance RangeMinMaxLevel RangeMinMaxL3 where
  rmmFactor _ = 32
  rmmBinWords v = rmmFactor v * rmmBinWords (rmmBase v)
  rmmBins = DVS.length . rangeMinMaxL3Min
  {-# INLINE rmmFactor    #-}
  {-# INLINE rmmBinWords  #-}
  {-# INLINE rmmBins      #-}

class MkRangeMinMaxL3 a where
  mkRangeMinMaxL3 :: a -> RangeMinMaxL3

instance MkRangeMinMaxL3 RangeMinMaxL2 where
  mkRangeMinMaxL3 rmmL2 = result
    where bp            = rangeMinMaxSimpleBP (rangeMinMaxSimple (rangeMinMaxL1Base (rangeMinMaxL2Base rmmL2)))
          lenL3         = (DVS.length (rangeMinMaxL2Min rmmL2) `div` rmmFactor result) + 1 :: Int
          allMinMaxL3   = DV.constructN lenL3 genMinMaxL3
          genMinMaxL3 v = let len = DV.length v in minMaxExcess1 (DVS.take (rmmBinWords result) (DVS.drop (len * rmmBinWords result) bp))
          rangeMinMaxL3ExcessA = DVS.constructN lenL3 (\v -> let (_, e,    _) = allMinMaxL3 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int16
          result = RangeMinMaxL3
            { rangeMinMaxL3Base     = rmmL2
            , rangeMinMaxL3Min      = DVS.constructN lenL3 (\v -> let (minE, _, _) = allMinMaxL3 DV.! DVS.length v in fromIntegral minE)
            , rangeMinMaxL3Max      = DVS.constructN lenL3 (\v -> let (_, _, maxE) = allMinMaxL3 DV.! DVS.length v in fromIntegral maxE)
            , rangeMinMaxL3Excess   = rangeMinMaxL3ExcessA
            }

instance MkRangeMinMaxL3 RangeMinMaxSimple where
  mkRangeMinMaxL3 = mkRangeMinMaxL3 . mkRangeMinMaxL2

instance MkRangeMinMaxL3 (DVS.Vector Word64) where
  mkRangeMinMaxL3 = mkRangeMinMaxL3 . mkRangeMinMaxSimple
  {-# INLINE mkRangeMinMaxL3 #-}

instance TestBit RangeMinMaxL3 where
  (.?.) = (.?.) . rangeMinMaxL3Base
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMaxL3 where
  rank1 = rank1 . rangeMinMaxL3Base
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMaxL3 where
  rank0 = rank0 . rangeMinMaxL3Base
  {-# INLINE rank0 #-}

instance BitLength RangeMinMaxL3 where
  bitLength = bitLength . rangeMinMaxL3Base
  {-# INLINE bitLength #-}

instance RangeMinMaxDerived RangeMinMaxL3 where
  type RangeMinMaxBase RangeMinMaxL3 = RangeMinMaxL2
  rmmBase = rangeMinMaxL3Base
  {-# INLINE rmmBase #-}

instance RangeMinMax RangeMinMaxL3 where
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
    where mins                  = rangeMinMaxL3Min v
          excesses              = rangeMinMaxL3Excess v
  {-# INLINE rmmFindCloseDispatch #-}
  {-# INLINE rmmFindCloseN        #-}

instance OpenAt RangeMinMaxL3 where
  openAt = openAt . rangeMinMaxL3Base
  {-# INLINE openAt #-}

instance CloseAt RangeMinMaxL3 where
  closeAt = closeAt . rangeMinMaxL3Base
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMaxL3 where
  newCloseAt = newCloseAt . rangeMinMaxL3Base
  {-# INLINE newCloseAt #-}

instance FindOpenN RangeMinMaxL3 where
  findOpenN = findOpenN . rangeMinMaxL3Base
  {-# INLINE findOpenN #-}

instance FindCloseN RangeMinMaxL3 where
  findCloseN v s p = (+ 1) `fmap` rmmFindClose v (fromIntegral s) (p - 1)
  {-# INLINE findCloseN #-}

instance FindOpen RangeMinMaxL3 where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose RangeMinMaxL3 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose RangeMinMaxL3 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens RangeMinMaxL3
