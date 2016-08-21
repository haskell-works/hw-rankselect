{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.Internal
  ( BalancedParens(..)
  , FindOpen(..)
  , FindClose(..)
  , depth
  , subtreeSize
  ) where

import           Control.Monad
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.Broadword
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.Enclose
import           HaskellWorks.Data.Succinct.BalancedParens.FindCloseN
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpenN
import           HaskellWorks.Data.Succinct.BalancedParens.Internal.Broadword
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1

class FindOpen v where
  findOpen    :: v -> Count -> Maybe Count

class FindClose v where
  findClose   :: v -> Count -> Maybe Count

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

-----

instance (FindOpen a) => FindOpen (BitShown a) where
  findOpen = findOpen . bitShown
  {-# INLINE findOpen #-}

instance (FindClose a) => FindClose (BitShown a) where
  findClose = findClose . bitShown
  {-# INLINE findClose #-}

instance FindOpen [Bool] where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose [Bool] where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance BalancedParens [Bool]

instance FindOpen (DVS.Vector Word8) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (DVS.Vector Word8) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance BalancedParens (DVS.Vector Word8)

instance FindOpen (DVS.Vector Word16) where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (DVS.Vector Word16) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance BalancedParens (DVS.Vector Word16)

instance FindOpen (DVS.Vector Word32) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (DVS.Vector Word32) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance BalancedParens (DVS.Vector Word32)

instance FindOpen (DVS.Vector Word64) where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (DVS.Vector Word64) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance BalancedParens (DVS.Vector Word64)

instance FindOpen Word8 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose Word8 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance BalancedParens Word8

instance FindOpen Word16 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose Word16 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance BalancedParens Word16

instance FindOpen Word32 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose Word32 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance BalancedParens Word32

instance FindOpen Word64 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose Word64 where
  findClose = findClose . Broadword
  {-# INLINE findClose #-}

instance BalancedParens Word64

instance FindOpen (Naive Word64) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (Naive Word64) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance BalancedParens (Naive Word64)

instance FindClose (Broadword Word64) where
  findClose (Broadword w) p = let x = w .>. (p - 1) in
    case negate (x .&. 1) .&. findCloseW64 x of
      127 -> Nothing
      r   -> let r' = fromIntegral r + p in if r' > 64 then Nothing else Just r'
  {-# INLINE findClose #-}
