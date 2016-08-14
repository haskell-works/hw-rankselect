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
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
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


instance OpenAt RangeMinMaxL0 where
  openAt = openAt . rangeMinMaxSimple
  {-# INLINE openAt #-}

instance CloseAt RangeMinMaxL0 where
  closeAt = closeAt . rangeMinMaxSimple
  {-# INLINE closeAt #-}

instance RangeMinMax RangeMinMaxL0 where
  rmmFindCloseDispatch v s p = if (p - 1) `mod` 64 == 0
    then rmmFindCloseN v s p
    else rmmFindCloseDispatch (rangeMinMaxSimple v) s p
  rmmFindCloseN v s p =
    let i = (p - 1) `div` 64 in
    let minE = fromIntegral (mins !!! fromIntegral i) :: Int in
    if fromIntegral s + minE <= 0
      then  rmmFindCloseN (rangeMinMaxSimple v) s p
      else if v `closeAt` p && s <= 1
        then Progress p
        else let excess  = fromIntegral (excesses !!! fromIntegral i)  :: Int in
              rmmFindClose v (fromIntegral (excess + fromIntegral s)) (p + 64)
    where mins                  = rangeMinMaxL0Min v
          excesses              = rangeMinMaxL0Excess v
  {-# INLINE rmmFindCloseDispatch #-}
  {-# INLINE rmmFindCloseN        #-}

instance BalancedParens RangeMinMaxL0 where
  -- findOpenN         = findOpenN   . rangeMinMaxBP
  findCloseN v s p = resultToMaybe (rmmFindClose v (fromIntegral s) p)

  -- {-# INLINE findOpenN   #-}
  {-# INLINE findCloseN  #-}
