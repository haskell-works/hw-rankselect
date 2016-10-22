{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.BalancedParens
  ( BalancedParens(..)
  , depth
  , subtreeSize
  ) where

import           Control.Monad
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.RankSelect.Base.Rank0
import           HaskellWorks.Data.RankSelect.Base.Rank1
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.Enclose
import           HaskellWorks.Data.Succinct.BalancedParens.FindClose
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpen
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt

class (OpenAt v, CloseAt v, FindOpen v, FindClose v, Enclose v) => BalancedParens v where
  -- TODO Second argument should be Int
  firstChild  :: v -> Count -> Maybe Count
  nextSibling :: v -> Count -> Maybe Count
  parent      :: v -> Count -> Maybe Count
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p
    then Nothing
    else openAt v `mfilter` (findClose v p >>= (\q ->
      if p /= q
        then return (q + 1)
        else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r      else Nothing)
  {-# INLINE firstChild   #-}
  {-# INLINE nextSibling  #-}
  {-# INLINE parent       #-}

depth :: (BalancedParens v, Rank0 v, Rank1 v) => v -> Count -> Maybe Count
depth v p = (\q -> rank1 v q - rank0 v q) <$> findOpen v p
{-# INLINE depth #-}

subtreeSize :: BalancedParens v => v -> Count -> Maybe Count
subtreeSize v p = (\q -> (q - p + 1) `quot` 2) <$> findClose v p
{-# INLINE subtreeSize #-}

instance BalancedParens [Bool]

instance BalancedParens (DVS.Vector Word8)

instance BalancedParens (DVS.Vector Word16)

instance BalancedParens (DVS.Vector Word32)

instance BalancedParens (DVS.Vector Word64)

instance BalancedParens Word8

instance BalancedParens Word16

instance BalancedParens Word32

instance BalancedParens Word64

instance BalancedParens (Naive Word64)
