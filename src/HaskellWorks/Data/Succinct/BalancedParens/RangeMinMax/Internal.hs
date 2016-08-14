module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
  ( RangeMinMax(..)
  , RangeMinMaxResult(..)
  , (<||>)
  ) where

import HaskellWorks.Data.Positioning

data RangeMinMaxResult a = Progress a | NoSkip | Fail
  deriving (Eq, Show)

class RangeMinMax v where
  rmmOpenAt   :: v -> Count -> Bool
  rmmCloseAt  :: v -> Count -> Bool
  rmmCloseN   :: v -> Int -> Count -> RangeMinMaxResult Count

(<||>) :: RangeMinMaxResult a -> RangeMinMaxResult a -> RangeMinMaxResult a
Fail        <||> _          = Fail
NoSkip      <||> b          = b
Progress a  <||> _          = Progress a
