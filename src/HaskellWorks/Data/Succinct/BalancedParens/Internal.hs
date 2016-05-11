module HaskellWorks.Data.Succinct.BalancedParens.Internal
  ( BalancedParens(..)
  , depth
  , subtreeSize
  ) where

import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1

class BalancedParens v where
  findOpen :: v -> Count -> Maybe Count
  findClose :: v -> Count -> Maybe Count
  enclose :: v -> Count -> Maybe Count
  firstChild :: v -> Count -> Maybe Count
  nextSibling :: v -> Count -> Maybe Count
  parent :: v -> Count -> Maybe Count

depth :: (BalancedParens v, Rank0 v, Rank1 v) => v -> Count -> Maybe Count
depth v p = (\q -> rank1 v q - rank0 v q) <$> findOpen v p

subtreeSize :: BalancedParens v => v -> Count -> Maybe Count
subtreeSize v p = (\q -> (q - p + 1) `quot` 2) <$> findClose v p
