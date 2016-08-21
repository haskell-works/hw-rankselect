{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.Internal
  ( BalancedParens(..)
  , Enclose(..)
  , FindOpen(..)
  , FindClose(..)
  , FindOpenN(..)
  , FindCloseN(..)
  , depth
  , subtreeSize
  ) where

import           Control.Monad
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.Broadword
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.Internal.Broadword
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1

class FindOpenN v where
  findOpenN :: v -> Count -> Count -> Maybe Count

class FindCloseN v where
  findCloseN :: v -> Count -> Count -> Maybe Count

class FindOpen v where
  findOpen    :: v -> Count -> Maybe Count

class FindClose v where
  findClose   :: v -> Count -> Maybe Count

class Enclose v where
  enclose     :: v -> Count -> Maybe Count

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

findOpen' :: (BitLength a, OpenAt a, TestBit a) => a -> Count -> Count -> Maybe Count
findOpen' v c p = if 0 < p && p <= bitLength v
  then if v `openAt` p
    then if c == 0
      then Just p
      else findOpen' v (c - 1) (p - 1)
    else findOpen' v (c + 1) (p - 1)
  else Nothing
{-# INLINE findOpen' #-}

findClose' :: (BitLength a, CloseAt a, TestBit a) => a -> Count -> Count -> Maybe Count
findClose' v c p = if 0 < p && p <= bitLength v
  then if v `closeAt` p
    then if c <= 1
      then Just p
      else findClose' v (c - 1) (p + 1)
    else findClose' v (c + 1) (p + 1)
  else Nothing
{-# INLINE findClose' #-}

instance (BalancedParens a, TestBit a, BitLength a) => FindOpenN (BitShown a) where
  findOpenN  = findOpen' . bitShown
  {-# INLINE findOpenN #-}

instance (BalancedParens a, TestBit a, BitLength a) => FindCloseN (BitShown a) where
  findCloseN = findClose' . bitShown
  {-# INLINE findCloseN #-}

instance (FindOpen a) => FindOpen (BitShown a) where
  findOpen = findOpen . bitShown
  {-# INLINE findOpen #-}

instance (FindClose a) => FindClose (BitShown a) where
  findClose = findClose . bitShown
  {-# INLINE findClose #-}

instance (Enclose a) => Enclose (BitShown a) where
  enclose = enclose . bitShown
  {-# INLINE enclose #-}

instance FindOpenN [Bool] where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindCloseN [Bool] where
  findCloseN = findClose'
  {-# INLINE findCloseN  #-}

instance FindOpen [Bool] where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose [Bool] where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose [Bool] where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens [Bool]

instance FindOpenN (DVS.Vector Word8) where
  findOpenN = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN (DVS.Vector Word8) where
  findCloseN = findClose'
  {-# INLINE findCloseN  #-}

instance FindOpen (DVS.Vector Word8) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (DVS.Vector Word8) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose (DVS.Vector Word8) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens (DVS.Vector Word8)

instance FindOpenN (DVS.Vector Word16) where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindCloseN (DVS.Vector Word16) where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindOpen (DVS.Vector Word16) where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (DVS.Vector Word16) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose (DVS.Vector Word16) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens (DVS.Vector Word16)

instance FindOpenN (DVS.Vector Word32) where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindCloseN (DVS.Vector Word32) where
  findCloseN = findClose'
  {-# INLINE findCloseN  #-}

instance FindOpen (DVS.Vector Word32) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (DVS.Vector Word32) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose (DVS.Vector Word32) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens (DVS.Vector Word32)

instance FindOpenN (DVS.Vector Word64) where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindCloseN (DVS.Vector Word64) where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindOpen (DVS.Vector Word64) where
  findOpen v p = if v `openAt` p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (DVS.Vector Word64) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose (DVS.Vector Word64) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens (DVS.Vector Word64)

instance FindOpenN Word8 where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindCloseN Word8 where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindOpen Word8 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose Word8 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose Word8 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens Word8

instance FindOpenN Word16 where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindCloseN Word16 where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindOpen Word16 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose Word16 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose Word16 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens Word16

instance FindOpenN Word32 where
  findOpenN = findOpen'
  {-# INLINE findOpenN   #-}

instance FindCloseN Word32 where
  findCloseN = findClose'
  {-# INLINE findCloseN  #-}

instance FindOpen Word32 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose Word32 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose Word32 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens Word32

instance FindOpenN Word64 where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindCloseN Word64 where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindOpen Word64 where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose Word64 where
  findClose = findClose . Broadword
  {-# INLINE findClose #-}

instance Enclose Word64 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens Word64

instance FindOpenN (Naive Word64) where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindCloseN (Naive Word64) where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindOpen (Naive Word64) where
  findOpen v p = if v `openAt`  p then Just p else findOpenN  v (Count 0) (p - 1)
  {-# INLINE findOpen #-}

instance FindClose (Naive Word64) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance Enclose (Naive Word64) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance BalancedParens (Naive Word64)

instance FindClose (Broadword Word64) where
  findClose (Broadword w) p = let x = w .>. (p - 1) in
    case negate (x .&. 1) .&. findCloseW64 x of
      127 -> Nothing
      r   -> let r' = fromIntegral r + p in if r' > 64 then Nothing else Just r'
  {-# INLINE findClose #-}
