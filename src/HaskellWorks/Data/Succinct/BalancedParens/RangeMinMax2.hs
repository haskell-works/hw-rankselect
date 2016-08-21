{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE TypeFamilies       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2
  ( RangeMinMax2(..)
  , RangeMinMax2Derived(..)
  , RangeMinMax2Level(..)
  , mkRangeMinMax2
  ) where

import qualified Data.Vector.Storable                                           as DVS
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
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           HaskellWorks.Data.Succinct.BalancedParens.NewCloseAt
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1

class RangeMinMax2Level v where
  rmm2Factor   :: v -> Int
  rmm2BinWords :: v -> Int
  rmm2Bins     :: v -> Int
  rmm2BinBits  :: v -> Count
  rmm2BinBits v = fromIntegral (rmm2BinWords v * 64)
  {-# INLINE rmm2BinBits #-}

class RangeMinMax2Derived v where
  type RangeMinMax2Base v
  rmm2Base :: v -> RangeMinMax2Base v

data RangeMinMax2 = RangeMinMax2
  { rangeMinMax2BP :: !(DVS.Vector Word64)
  }

mkRangeMinMax2 :: DVS.Vector Word64 -> RangeMinMax2
mkRangeMinMax2 bp = RangeMinMax2
  { rangeMinMax2BP = bp
  }

rmm2FindClose  :: RangeMinMax2 -> Int -> Count -> Maybe Count
rmm2FindClose v s p = if 0 <= p && p < bitLength v
  then if v `newCloseAt` p
    then if s <= 1
      then Just p
      else rmm2FindClose v (s - 1) (p + 1)
    else rmm2FindClose v (s + 1) (p + 1)
  else Nothing
{-# INLINE rmm2FindClose #-}

instance TestBit RangeMinMax2 where
  (.?.) = (.?.) . rangeMinMax2BP
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMax2 where
  rank1 = rank1 . rangeMinMax2BP
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMax2 where
  rank0 = rank0 . rangeMinMax2BP
  {-# INLINE rank0 #-}

instance BitLength RangeMinMax2 where
  bitLength = bitLength . rangeMinMax2BP
  {-# INLINE bitLength #-}

instance OpenAt RangeMinMax2 where
  openAt = openAt . rangeMinMax2BP
  {-# INLINE openAt #-}

instance CloseAt RangeMinMax2 where
  closeAt = closeAt . rangeMinMax2BP
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMax2 where
  newCloseAt = newCloseAt . rangeMinMax2BP
  {-# INLINE newCloseAt #-}

instance FindOpenN RangeMinMax2 where
  findOpenN         = findOpenN . rangeMinMax2BP
  {-# INLINE findOpenN   #-}

instance FindCloseN RangeMinMax2 where
  findCloseN v s p  = (+ 1) `fmap` rmm2FindClose v (fromIntegral s) (p - 1)
  {-# INLINE findCloseN  #-}

instance FindClose RangeMinMax2 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindOpen RangeMinMax2 where
  findOpen = undefined
  {-# INLINE findOpen #-}

instance Enclose RangeMinMax2 where
  enclose = undefined
  {-# INLINE enclose #-}

instance BalancedParens RangeMinMax2
