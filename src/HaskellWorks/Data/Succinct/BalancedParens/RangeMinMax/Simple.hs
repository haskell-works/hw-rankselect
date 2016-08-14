{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Simple
  ( RangeMinMaxSimple(..)
  , mkRangeMinMaxSimple
  ) where

import           Data.Int
import qualified Data.Vector                                        as DV
import qualified Data.Vector.Storable                               as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMaxSimple = RangeMinMaxSimple
  { rangeMinMaxSimpleBP     :: DVS.Vector Word64
  , rangeMinMaxSimpleMin    :: DVS.Vector Int8
  , rangeMinMaxSimpleMax    :: DVS.Vector Int8
  , rangeMinMaxSimpleExcess :: DVS.Vector Int8
  }

mkRangeMinMaxSimple :: DVS.Vector Word64 -> RangeMinMaxSimple
mkRangeMinMaxSimple bp = RangeMinMaxSimple
  { rangeMinMaxSimpleBP       = bp
  , rangeMinMaxSimpleMin    = DVS.constructN (len0 + 1) (\v -> let (minE, _, _) = allMinMax DV.! DVS.length v in fromIntegral minE)
  , rangeMinMaxSimpleMax    = DVS.constructN (len0 + 1) (\v -> let (_, _, maxE) = allMinMax DV.! DVS.length v in fromIntegral maxE)
  , rangeMinMaxSimpleExcess = DVS.constructN (len0 + 1) (\v -> let (_, e,    _) = allMinMax DV.! DVS.length v in fromIntegral e)
  }
  where len0        = fromIntegral (vLength bp) :: Int
        allMinMax   = DV.constructN (len0 + 1) genMinMax
        genMinMax v = let len = DV.length v in
                      if len == len0
                        then (0, 0, 0)
                        else minMaxExcess1 (bp !!! fromIntegral len)

instance TestBit RangeMinMaxSimple where
  (.?.) = (.?.) . rangeMinMaxSimpleBP
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMaxSimple where
  rank1 = rank1 . rangeMinMaxSimpleBP
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMaxSimple where
  rank0 = rank0 . rangeMinMaxSimpleBP
  {-# INLINE rank0 #-}

instance BitLength RangeMinMaxSimple where
  bitLength = bitLength . rangeMinMaxSimpleBP
  {-# INLINE bitLength #-}

rangeMinMaxFindCloseN :: RangeMinMaxSimple -> Int -> Count -> Maybe Count
rangeMinMaxFindCloseN v s p  = result
  where bp                    = rangeMinMaxSimpleBP v
        mins                  = rangeMinMaxSimpleMin v
        excesses              = rangeMinMaxSimpleExcess v
        findCloseN'           = if v `closeAt` p
          then if s <= 1
            then Just p
            else rangeMinMaxFindCloseN v (s - 1) (p + 1)
          else rangeMinMaxFindCloseN v (s + 1) (p + 1)
        result                = if 0 < p && p <= bitLength v
          then if (p - 1) `mod` elemBitLength bp == 0
            then  let i = (p - 1) `div` elemBitLength bp in
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

instance BalancedParens RangeMinMaxSimple where
  openAt            = openAt      . rangeMinMaxSimpleBP
  closeAt           = closeAt     . rangeMinMaxSimpleBP
  -- findOpenN         = findOpenN   . rangeMinMaxBP
  findCloseN v s    = rangeMinMaxFindCloseN v (fromIntegral s)

  {-# INLINE openAt      #-}
  {-# INLINE closeAt     #-}
  -- {-# INLINE findOpenN   #-}
  {-# INLINE findCloseN  #-}
