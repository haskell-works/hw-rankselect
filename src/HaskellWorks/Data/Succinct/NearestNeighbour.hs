module HaskellWorks.Data.Succinct.NearestNeighbour
  ( bitPred
  , bitSucc
  ) where

import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1

bitPred :: (Rank1 v, Select1 v) => v -> Count -> Count
bitPred v p = select1 v (rank1 v p - 1)

bitSucc :: (Rank1 v, Select1 v) => v -> Count -> Count
bitSucc v p = select1 v (rank1 v p + 1)
