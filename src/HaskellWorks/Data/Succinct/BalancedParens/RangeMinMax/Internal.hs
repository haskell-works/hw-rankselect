{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
  ( RangeMinMax(..)
  , RangeMinMaxDerived(..)
  ) where

import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal

class RangeMinMaxDerived v where
  type RangeMinMaxBase v
  rmmBase :: v -> RangeMinMaxBase v

class (OpenAt v, CloseAt v, BitLength v) => RangeMinMax v where
  rmmFindCloseDispatch :: v -> Int -> Count -> Maybe Count
  rmmFindCloseN :: v -> Int -> Count -> Maybe Count
  rmmFindClose  :: v -> Int -> Count -> Maybe Count
  rmmFindClose v s p = if 0 < p && p <= bitLength v
    then rmmFindCloseDispatch v s p
    else Nothing
  {-# INLINE rmmFindClose #-}
