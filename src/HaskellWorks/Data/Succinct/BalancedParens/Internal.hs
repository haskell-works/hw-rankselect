module HaskellWorks.Data.Succinct.BalancedParens.Internal
  ( BalancedParens(..)
  , depth
  , subtreeSize
  ) where

import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1

class BalancedParens v where
  findOpen :: v -> Count -> Count
  findClose :: v -> Count -> Count
  enclose :: v -> Count -> Count
  firstChild :: v -> Count -> Maybe Count
  nextSibling :: v -> Count -> Maybe Count
  parent :: v -> Count -> Maybe Count

depth :: (BalancedParens v, Rank0 v, Rank1 v) => v -> Count -> Count
depth v p = let q = findOpen v p in rank1 v q - rank0 v q

subtreeSize :: BalancedParens v => v -> Count -> Count
subtreeSize v p = (findClose v p - p + 1) `quot` 2
