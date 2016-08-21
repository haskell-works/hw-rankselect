{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.FindOpenN
  ( FindOpenN(..)
  ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt

class FindOpenN v where
  findOpenN :: v -> Count -> Count -> Maybe Count

findOpen' :: (BitLength a, OpenAt a, TestBit a) => a -> Count -> Count -> Maybe Count
findOpen' v c p = if 0 < p && p <= bitLength v
  then if v `openAt` p
    then if c == 0
      then Just p
      else findOpen' v (c - 1) (p - 1)
    else findOpen' v (c + 1) (p - 1)
  else Nothing
{-# INLINE findOpen' #-}

instance (BitLength a, OpenAt a, TestBit a) => FindOpenN (BitShown a) where
  findOpenN  = findOpen' . bitShown
  {-# INLINE findOpenN #-}

instance FindOpenN [Bool] where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindOpenN (DVS.Vector Word8) where
  findOpenN = findOpen'
  {-# INLINE findOpenN   #-}

instance FindOpenN (DVS.Vector Word16) where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindOpenN (DVS.Vector Word32) where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindOpenN (DVS.Vector Word64) where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindOpenN Word8 where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindOpenN Word16 where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindOpenN Word32 where
  findOpenN = findOpen'
  {-# INLINE findOpenN   #-}

instance FindOpenN Word64 where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}

instance FindOpenN (Naive Word64) where
  findOpenN = findOpen'
  {-# INLINE findOpenN #-}
