{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.L3
  ( RangeMinMax2L3(..)
  , mkRangeMinMax2L3
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
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.L1
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.L2
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMax2L3 = RangeMinMax2L3
  { rangeMinMaxL3Base     :: !RangeMinMax2L2
  , rangeMinMaxL3Min      :: !(DVS.Vector Int16)
  , rangeMinMaxL3Max      :: !(DVS.Vector Int16)
  , rangeMinMaxL3Excess   :: !(DVS.Vector Int16)
  }

instance RangeMinMax2Level RangeMinMax2L3 where
  rmmFactor _ = 32
  rmmBinWords v = rmmFactor v * rmmBinWords (rmmBase v)
  rmmBins = DVS.length . rangeMinMaxL3Min
  {-# INLINE rmmFactor    #-}
  {-# INLINE rmmBinWords  #-}
  {-# INLINE rmmBins      #-}

class MkRangeMinMax2L3 a where
  mkRangeMinMax2L3 :: a -> RangeMinMax2L3

instance MkRangeMinMax2L3 RangeMinMax2L2 where
  mkRangeMinMax2L3 rmmL2 = result
    where bp            = rangeMinMaxSimpleBP (rangeMinMaxSimple (rangeMinMaxL1Base (rangeMinMaxL2Base rmmL2)))
          lenL3         = (DVS.length (rangeMinMaxL2Min rmmL2) `div` rmmFactor result) + 1 :: Int
          allMinMaxL3   = DV.constructN lenL3 genMinMaxL3
          genMinMaxL3 v = let len = DV.length v in minMaxExcess1 (DVS.take (rmmBinWords result) (DVS.drop (len * rmmBinWords result) bp))
          rangeMinMaxL3ExcessA = DVS.constructN lenL3 (\v -> let (_, e,    _) = allMinMaxL3 DV.! DVS.length v in fromIntegral e) :: DVS.Vector Int16
          result = RangeMinMax2L3
            { rangeMinMaxL3Base     = rmmL2
            , rangeMinMaxL3Min      = DVS.constructN lenL3 (\v -> let (minE, _, _) = allMinMaxL3 DV.! DVS.length v in fromIntegral minE)
            , rangeMinMaxL3Max      = DVS.constructN lenL3 (\v -> let (_, _, maxE) = allMinMaxL3 DV.! DVS.length v in fromIntegral maxE)
            , rangeMinMaxL3Excess   = rangeMinMaxL3ExcessA
            }
  {-# INLINE mkRangeMinMax2L3 #-}

instance MkRangeMinMax2L3 RangeMinMax2Simple where
  mkRangeMinMax2L3 = mkRangeMinMax2L3 . mkRangeMinMax2L2
  {-# INLINE mkRangeMinMax2L3 #-}

instance MkRangeMinMax2L3 (DVS.Vector Word64) where
  mkRangeMinMax2L3 = mkRangeMinMax2L3 . mkRangeMinMax2Simple
  {-# INLINE mkRangeMinMax2L3 #-}

instance TestBit RangeMinMax2L3 where
  (.?.) = (.?.) . rangeMinMaxL3Base
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMax2L3 where
  rank1 = rank1 . rangeMinMaxL3Base
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMax2L3 where
  rank0 = rank0 . rangeMinMaxL3Base
  {-# INLINE rank0 #-}

instance BitLength RangeMinMax2L3 where
  bitLength = bitLength . rangeMinMaxL3Base
  {-# INLINE bitLength #-}

instance RangeMinMax2Derived RangeMinMax2L3 where
  type RangeMinMax2Base RangeMinMax2L3 = RangeMinMax2L2
  rmmBase = rangeMinMaxL3Base
  {-# INLINE rmmBase #-}

instance RangeMinMax2 RangeMinMax2L3 where
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

instance OpenAt RangeMinMax2L3 where
  openAt = openAt . rangeMinMaxL3Base
  {-# INLINE openAt #-}

instance CloseAt RangeMinMax2L3 where
  closeAt = closeAt . rangeMinMaxL3Base
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMax2L3 where
  newCloseAt = newCloseAt . rangeMinMaxL3Base
  {-# INLINE newCloseAt #-}

instance FindOpenN RangeMinMax2L3 where
  findOpenN = findOpenN . rangeMinMaxL3Base
  {-# INLINE findOpenN #-}

instance FindCloseN RangeMinMax2L3 where
  findCloseN v s p = (+ 1) `fmap` rmmFindClose v (fromIntegral s) (p - 1)
  {-# INLINE findCloseN #-}

instance FindOpen RangeMinMax2L3 where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose RangeMinMax2L3 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose RangeMinMax2L3 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens RangeMinMax2L3
