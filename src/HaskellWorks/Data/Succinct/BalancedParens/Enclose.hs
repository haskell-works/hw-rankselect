{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.Enclose
  ( Enclose(..)
  ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpenN

class Enclose v where
  enclose     :: v -> Count -> Maybe Count

instance (Enclose a) => Enclose (BitShown a) where
  enclose = enclose . bitShown
  {-# INLINE enclose #-}

instance Enclose [Bool] where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance Enclose (DVS.Vector Word8) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance Enclose (DVS.Vector Word16) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance Enclose (DVS.Vector Word32) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance Enclose (DVS.Vector Word64) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance Enclose Word8 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance Enclose Word16 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance Enclose Word32 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance Enclose Word64 where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}

instance Enclose (Naive Word64) where
  enclose v = findOpenN v (Count 1)
  {-# INLINE enclose #-}
