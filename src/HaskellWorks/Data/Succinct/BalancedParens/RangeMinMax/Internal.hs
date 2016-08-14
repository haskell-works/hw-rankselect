module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.Internal
  ( RangeMinMax(..)
  , RangeMinMaxResult(..)
  , resultToMaybe
  , (<||>)
  ) where

import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal

data RangeMinMaxResult a = Progress a | NoSkip | Fail
  deriving (Eq, Show)

class (OpenAt v, CloseAt v) => RangeMinMax v where
  rmmFindCloseN   :: v -> Int -> Count -> RangeMinMaxResult Count

(<||>) :: RangeMinMaxResult a -> RangeMinMaxResult a -> RangeMinMaxResult a
Fail        <||> _          = Fail
NoSkip      <||> b          = b
Progress a  <||> _          = Progress a

resultToMaybe :: RangeMinMaxResult a -> Maybe a
resultToMaybe Fail          = Nothing
resultToMaybe NoSkip        = Nothing
resultToMaybe (Progress a)  = Just a
