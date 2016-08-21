{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.OpenAt
  ( OpenAt(..)
  ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.Broadword
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Positioning

class OpenAt v where
  openAt      :: v -> Count -> Bool

openAt' :: (BitLength a, TestBit a) => a -> Count -> Bool
openAt' v c = (0 <= c && c < bitLength v) && (v .?. toPosition (c - 1))
{-# INLINE openAt' #-}

instance (BitLength a, TestBit a) => OpenAt (BitShown a) where
  openAt = openAt' . bitShown
  {-# INLINE openAt #-}

instance OpenAt [Bool] where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt (DVS.Vector Word8) where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt (DVS.Vector Word16) where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt (DVS.Vector Word32) where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt (DVS.Vector Word64) where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt Word8 where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt Word16 where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt Word32 where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt Word64 where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt (Naive Word64) where
  openAt = openAt'
  {-# INLINE openAt #-}

instance OpenAt (Broadword Word64) where
  openAt = openAt . broadword
  {-# INLINE openAt #-}
