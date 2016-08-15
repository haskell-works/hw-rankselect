{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
  ( RangeMinMax(..)
  , RangeMinMaxDerived(..)
  , RangeMinMaxLevel(..)
  , (<||>)
  ) where

import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal

class RangeMinMaxLevel v where
  rmmFactor   :: v -> Int
  rmmBinWords :: v -> Int
  rmmBins     :: v -> Int
  rmmBinBits  :: v -> Count
  rmmBinBits v = fromIntegral (rmmBinWords v * 64)
  {-# INLINE rmmBinBits #-}

class RangeMinMaxDerived v where
  type RangeMinMaxBase v
  rmmBase :: v -> RangeMinMaxBase v

class (OpenAt v, CloseAt v, BitLength v) => RangeMinMax v where
  rmmFindCloseDispatch :: v -> Int -> Count -> Maybe Count
  rmmFindCloseN :: v -> Int -> Count -> Maybe Count
  rmmFindClose  :: v -> Int -> Count -> Maybe Count
  rmmFindClose v s p = if 0 <= p && p < bitLength v
    then rmmFindCloseDispatch v s p
    else Nothing
  {-# INLINE rmmFindClose #-}

(<||>) :: Maybe a -> Maybe a -> Maybe a
(<||>) ma mb = case ma of
  Just _  -> ma
  Nothing -> mb
