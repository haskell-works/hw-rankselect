{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.Internal
  ( BalancedParens(..)
  , FindOpenN(..)
  , FindCloseN(..)
  , OpenAt(..)
  , CloseAt(..)
  , depth
  , subtreeSize
  ) where

import           Control.Monad
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1

class OpenAt v where
  openAt      :: v -> Count -> Bool

class CloseAt v where
  closeAt     :: v -> Count -> Bool

class FindOpenN v where
  findOpenN :: v -> Count -> Count -> Maybe Count

class FindCloseN v where
  findCloseN :: v -> Count -> Count -> Maybe Count

class (OpenAt v, CloseAt v) => BalancedParens v where
  -- TODO Second argument should be Int
  enclose     :: v -> Count -> Maybe Count
  firstChild  :: v -> Count -> Maybe Count
  nextSibling :: v -> Count -> Maybe Count
  parent      :: v -> Count -> Maybe Count
  findOpen    :: v -> Count -> Maybe Count
  findClose   :: v -> Count -> Maybe Count
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

subtreeSize :: BalancedParens v => v -> Count -> Maybe Count
subtreeSize v p = (\q -> (q - p + 1) `quot` 2) <$> findClose v p

closeAt' :: TestBit a => a -> Count -> Bool
closeAt' v c = not (v .?. toPosition (c - 1))
{-# INLINE closeAt' #-}

openAt' :: (BitLength a, TestBit a) => a -> Count -> Bool
openAt' v c = (0 <= c && c < bitLength v) && (v .?. toPosition (c - 1))
{-# INLINE openAt' #-}

-----

findOpen' :: (BitLength a, TestBit a) => a -> Count -> Count -> Maybe Count
findOpen' v c p = if 0 < p && p <= bitLength v
  then if v `openAt'` p
    then if c == 0
      then Just p
      else findOpen' v (c - 1) (p - 1)
    else findOpen' v (c + 1) (p - 1)
  else Nothing
{-# INLINE findOpen' #-}

findClose' :: (BitLength a, TestBit a) => a -> Count -> Count -> Maybe Count
findClose' v c p = if 0 < p && p <= bitLength v
  then if v `closeAt'` p
    then if c <= 1
      then Just p
      else findClose' v (c - 1) (p + 1)
    else findClose' v (c + 1) (p + 1)
  else Nothing
{-# INLINE findClose' #-}

instance (BitLength a, TestBit a) => OpenAt (BitShown a) where
  openAt = openAt' . bitShown
  {-# INLINE openAt      #-}

instance (BitLength a, TestBit a) => CloseAt (BitShown a) where
  closeAt = closeAt' . bitShown
  {-# INLINE closeAt     #-}

instance (BalancedParens a, TestBit a, BitLength a) => FindOpenN (BitShown a) where
  findOpenN  = findOpen' . bitShown
  {-# INLINE findOpenN   #-}

instance (BalancedParens a, TestBit a, BitLength a) => FindCloseN (BitShown a) where
  findCloseN = findClose' . bitShown
  {-# INLINE findCloseN  #-}

instance (BalancedParens a, TestBit a, BitLength a) => BalancedParens (BitShown a) where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}

instance OpenAt [Bool] where
  openAt = openAt'
  {-# INLINE openAt      #-}

instance CloseAt [Bool] where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance FindOpenN [Bool] where
  findOpenN = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN [Bool] where
  findCloseN = findClose'
  {-# INLINE findCloseN  #-}

instance BalancedParens [Bool] where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}

instance OpenAt (DVS.Vector Word8) where
  openAt = openAt'
  {-# INLINE openAt      #-}

instance CloseAt (DVS.Vector Word8) where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance FindOpenN (DVS.Vector Word8) where
  findOpenN       = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN (DVS.Vector Word8) where
  findCloseN      = findClose'
  {-# INLINE findCloseN  #-}

instance BalancedParens (DVS.Vector Word8) where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}

instance OpenAt (DVS.Vector Word16) where
  openAt = openAt'
  {-# INLINE openAt      #-}

instance CloseAt (DVS.Vector Word16) where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance FindOpenN (DVS.Vector Word16) where
  findOpenN       = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN (DVS.Vector Word16) where
  findCloseN      = findClose'
  {-# INLINE findCloseN  #-}

instance BalancedParens (DVS.Vector Word16) where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}

instance OpenAt (DVS.Vector Word32) where
  openAt = openAt'
  {-# INLINE openAt      #-}

instance CloseAt (DVS.Vector Word32) where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance FindOpenN (DVS.Vector Word32) where
  findOpenN       = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN (DVS.Vector Word32) where
  findCloseN      = findClose'
  {-# INLINE findCloseN  #-}

instance BalancedParens (DVS.Vector Word32) where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}

instance OpenAt (DVS.Vector Word64) where
  openAt = openAt'
  {-# INLINE openAt      #-}

instance CloseAt (DVS.Vector Word64) where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance FindOpenN (DVS.Vector Word64) where
  findOpenN       = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN (DVS.Vector Word64) where
  findCloseN      = findClose'
  {-# INLINE findCloseN  #-}

instance BalancedParens (DVS.Vector Word64) where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}

instance OpenAt Word8 where
  openAt = openAt'
  {-# INLINE openAt      #-}

instance CloseAt Word8 where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance FindOpenN Word8 where
  findOpenN       = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN Word8 where
  findCloseN      = findClose'
  {-# INLINE findCloseN  #-}

instance BalancedParens Word8 where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}

instance OpenAt Word16 where
  openAt = openAt'
  {-# INLINE openAt      #-}

instance CloseAt Word16 where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance FindOpenN Word16 where
  findOpenN       = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN Word16 where
  findCloseN      = findClose'
  {-# INLINE findCloseN  #-}

instance BalancedParens Word16 where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}

instance OpenAt Word32 where
  openAt = openAt'
  {-# INLINE openAt      #-}

instance CloseAt Word32 where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance FindOpenN Word32 where
  findOpenN       = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN Word32 where
  findCloseN      = findClose'
  {-# INLINE findCloseN  #-}

instance BalancedParens Word32 where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}

instance OpenAt Word64 where
  openAt = openAt'
  {-# INLINE openAt      #-}

instance CloseAt Word64 where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance FindOpenN Word64 where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindCloseN Word64 where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance BalancedParens Word64 where
  findOpen    v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  enclose     v   = findOpenN v (Count 1)
  {-# INLINE findOpen     #-}
  {-# INLINE findClose    #-}
  {-# INLINE enclose      #-}
