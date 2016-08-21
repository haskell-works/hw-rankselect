{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.FindCloseN
  ( FindCloseN(..)
  ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt

class FindCloseN v where
  findCloseN :: v -> Count -> Count -> Maybe Count

findClose' :: (BitLength a, CloseAt a, TestBit a) => a -> Count -> Count -> Maybe Count
findClose' v c p = if 0 < p && p <= bitLength v
  then if v `closeAt` p
    then if c <= 1
      then Just p
      else findClose' v (c - 1) (p + 1)
    else findClose' v (c + 1) (p + 1)
  else Nothing
{-# INLINE findClose' #-}

instance (CloseAt a, TestBit a, BitLength a) => FindCloseN (BitShown a) where
  findCloseN = findClose' . bitShown
  {-# INLINE findCloseN #-}

instance FindCloseN [Bool] where
  findCloseN = findClose'
  {-# INLINE findCloseN  #-}

instance FindCloseN (DVS.Vector Word8) where
  findCloseN = findClose'
  {-# INLINE findCloseN  #-}

instance FindCloseN (DVS.Vector Word16) where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindCloseN (DVS.Vector Word32) where
  findCloseN = findClose'
  {-# INLINE findCloseN  #-}

instance FindCloseN (DVS.Vector Word64) where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindCloseN Word8 where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindCloseN Word16 where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindCloseN Word32 where
  findCloseN = findClose'
  {-# INLINE findCloseN  #-}

instance FindCloseN Word64 where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}

instance FindCloseN (Naive Word64) where
  findCloseN = findClose'
  {-# INLINE findCloseN #-}
