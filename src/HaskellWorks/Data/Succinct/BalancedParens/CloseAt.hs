{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.CloseAt
  ( CloseAt(..)
  ) where

import           Data.Vector.Storable             as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.Broadword
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Naive

closeAt' :: TestBit a => a -> Count -> Bool
closeAt' v c = not (v .?. toPosition (c - 1))
{-# INLINE closeAt' #-}

class CloseAt v where
  closeAt     :: v -> Count -> Bool

instance (BitLength a, TestBit a) => CloseAt (BitShown a) where
  closeAt = closeAt' . bitShown
  {-# INLINE closeAt #-}

instance CloseAt [Bool] where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt (DVS.Vector Word8) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}


instance CloseAt (DVS.Vector Word16) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}


instance CloseAt (DVS.Vector Word32) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}


instance CloseAt (DVS.Vector Word64) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}


instance CloseAt Word8 where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt Word16 where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt Word32 where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt Word64 where
  closeAt = closeAt'
  {-# INLINE closeAt     #-}

instance CloseAt (Naive Word64) where
  closeAt = closeAt'
  {-# INLINE closeAt #-}

instance CloseAt (Broadword Word64) where
  closeAt = closeAt . broadword
  {-# INLINE closeAt #-}
