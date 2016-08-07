{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax
  ( RangeMinMax(..)
  , mkRangeMinMax
  ) where

import           Data.Int
import qualified Data.Vector                                              as DV
import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
-- import qualified HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L0 as L0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMax = RangeMinMax
  { rangeMinMaxBP       :: DVS.Vector Word64
  , rangeMinMaxL0Min    :: DVS.Vector Int16
  , rangeMinMaxL0Max    :: DVS.Vector Int16
  , rangeMinMaxL0Excess :: DVS.Vector Int16
  }

mkRangeMinMax :: DVS.Vector Word64 -> RangeMinMax
mkRangeMinMax bp = RangeMinMax
  { rangeMinMaxBP       = bp
  , rangeMinMaxL0Min    = DVS.constructN (len0 `div` 8 + 1) (\v -> let (minE, _, _) = allMinMax DV.! DVS.length v in fromIntegral minE)
  , rangeMinMaxL0Max    = DVS.constructN (len0 `div` 8 + 1) (\v -> let (_, _, maxE) = allMinMax DV.! DVS.length v in fromIntegral maxE)
  , rangeMinMaxL0Excess = DVS.constructN (len0 `div` 8 + 1) (\v -> let (_, e,    _) = allMinMax DV.! DVS.length v in fromIntegral e)
  }
  where len0        = fromIntegral (vLength bp) :: Int
        allMinMax   = DV.constructN (len0 `div` 8 + 1) genMinMax
        genMinMax v = let len = DV.length v in
                      minMaxExcess1 (DVS.take 8 (DVS.drop (fromIntegral len * 8) bp))

instance TestBit RangeMinMax where
  (.?.) = (.?.) . rangeMinMaxBP
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMax where
  rank1 = rank1 . rangeMinMaxBP
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMax where
  rank0 = rank0 . rangeMinMaxBP
  {-# INLINE rank0 #-}

instance BitLength RangeMinMax where
  bitLength = bitLength . rangeMinMaxBP
  {-# INLINE bitLength #-}

rangeMinMaxFindCloseN :: RangeMinMax -> Int -> Count -> Maybe Count
rangeMinMaxFindCloseN v s p  = result
  where mins                  = rangeMinMaxL0Min v
        excesses              = rangeMinMaxL0Excess v
        findCloseN'           = if v `closeAt` p
          then if s <= 1
            then Just p
            else rangeMinMaxFindCloseN v (s - 1) (p + 1)
          else rangeMinMaxFindCloseN v (s + 1) (p + 1)
        result                = if 0 < p && p <= bitLength v
          then if (p - 1) `mod` 512 == 0
            then  let i = (p - 1) `div` 512 in
                  let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
                  if fromIntegral s + minE <= 0
                    then  findCloseN'
                    else if v `closeAt` p && s <= 1
                      then Just p
                      else let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
                            rangeMinMaxFindCloseN v (fromIntegral (excess + fromIntegral s)) (p + 512)
            else findCloseN'
          else Nothing
{-# INLINE rangeMinMaxFindCloseN #-}

instance BalancedParens RangeMinMax where
  openAt            = openAt      . rangeMinMaxBP
  closeAt           = closeAt     . rangeMinMaxBP
  -- findOpenN         = findOpenN   . rangeMinMaxBP
  findCloseN v s    = rangeMinMaxFindCloseN v (fromIntegral s)

  {-# INLINE openAt      #-}
  {-# INLINE closeAt     #-}
  -- {-# INLINE findOpenN   #-}
  {-# INLINE findCloseN  #-}
