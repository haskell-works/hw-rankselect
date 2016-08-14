{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L0
  ( RangeMinMaxL0(..)
  , mkRangeMinMaxL0
  ) where

import           Data.Int
import qualified Data.Vector                                                  as DV
import qualified Data.Vector.Storable                                         as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMaxL0 = RangeMinMaxL0
  { rangeMinMaxSimple   :: RangeMinMaxSimple
  , rangeMinMaxL0Min    :: DVS.Vector Int8
  , rangeMinMaxL0Max    :: DVS.Vector Int8
  , rangeMinMaxL0Excess :: DVS.Vector Int8
  }

class MkRangeMinMaxL0 a where
  mkRangeMinMaxL0 :: a -> RangeMinMaxL0

instance MkRangeMinMaxL0 RangeMinMaxSimple where
  mkRangeMinMaxL0 simple = RangeMinMaxL0
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

instance MkRangeMinMaxL0 (DVS.Vector Word64) where
  mkRangeMinMaxL0 = mkRangeMinMaxL0 . mkRangeMinMaxSimple

instance TestBit RangeMinMaxL0 where
  (.?.) = (.?.) . rangeMinMaxSimple
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMaxL0 where
  rank1 = rank1 . rangeMinMaxSimple
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMaxL0 where
  rank0 = rank0 . rangeMinMaxSimple
  {-# INLINE rank0 #-}

instance BitLength RangeMinMaxL0 where
  bitLength = bitLength . rangeMinMaxSimple
  {-# INLINE bitLength #-}

rangeMinMaxFindCloseN :: RangeMinMaxL0 -> Int -> Count -> Maybe Count
rangeMinMaxFindCloseN v s p  = result
  where mins                  = rangeMinMaxL0Min v
        excesses              = rangeMinMaxL0Excess v
        findCloseN'           = if v `closeAt` p
          then if s <= 1
            then Just p
            else rangeMinMaxFindCloseN v (s - 1) (p + 1)
          else rangeMinMaxFindCloseN v (s + 1) (p + 1)
        result                = if 0 < p && p <= bitLength v
          then if (p - 1) `mod` 64 == 0
            then  let i = (p - 1) `div` 64 in
                  let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
                  if fromIntegral s + minE <= 0
                    then  findCloseN'
                    else if v `closeAt` p && s <= 1
                      then Just p
                      else let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
                            rangeMinMaxFindCloseN v (fromIntegral (excess + fromIntegral s)) (p + 64)
            else findCloseN'
          else Nothing
{-# INLINE rangeMinMaxFindCloseN #-}

instance OpenAt RangeMinMaxL0 where
  openAt = openAt . rangeMinMaxSimple
  {-# INLINE openAt #-}

instance CloseAt RangeMinMaxL0 where
  closeAt = closeAt . rangeMinMaxSimple
  {-# INLINE closeAt #-}

instance BalancedParens RangeMinMaxL0 where
  -- findOpenN         = findOpenN   . rangeMinMaxBP
  findCloseN v s    = rangeMinMaxFindCloseN v (fromIntegral s)

  -- {-# INLINE findOpenN   #-}
  {-# INLINE findCloseN  #-}
