{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
  ( RangeMinMax(..)
  , RangeMinMaxDerived(..)
  , RangeMinMaxResult(..)
  , resultToMaybe
  , (<||>)
  ) where

import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal

data RangeMinMaxResult a = Progress a | NoSkip | Fail
  deriving (Eq, Show)

class RangeMinMaxDerived v where
  type RangeMinMaxBase v
  rmmBase :: v -> RangeMinMaxBase v

class (OpenAt v, CloseAt v, BitLength v) => RangeMinMax v where
  rmmFindClose  :: v -> Int -> Count -> RangeMinMaxResult Count
  rmmFindClose v s p = if 0 < p && p <= bitLength v
    then rmmFindCloseDispatch v s p
    else Fail
  rmmFindCloseDispatch :: v -> Int -> Count -> RangeMinMaxResult Count
  rmmFindCloseN :: v -> Int -> Count -> RangeMinMaxResult Count
  {-# INLINE rmmFindClose #-}

(<||>) :: RangeMinMaxResult a -> RangeMinMaxResult a -> RangeMinMaxResult a
Fail        <||> _          = Fail
NoSkip      <||> b          = b
Progress a  <||> _          = Progress a

resultToMaybe :: RangeMinMaxResult a -> Maybe a
resultToMaybe Fail          = Nothing
resultToMaybe NoSkip        = Nothing
resultToMaybe (Progress a)  = Just a
