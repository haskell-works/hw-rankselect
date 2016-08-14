{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.NewOpenAt
  ( NewOpenAt(..)
  ) where

import qualified Data.Vector.Storable             as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning

class NewOpenAt v where
  newOpenAt      :: v -> Count -> Bool

newOpenAt' :: (BitLength a, TestBit a) => a -> Count -> Bool
newOpenAt' v c = (0 <= c && c < bitLength v) && (v .?. toPosition c)
{-# INLINE newOpenAt' #-}

instance (BitLength a, TestBit a) => NewOpenAt (BitShown a) where
  newOpenAt = newOpenAt' . bitShown
  {-# INLINE newOpenAt #-}

instance NewOpenAt [Bool] where
  newOpenAt = newOpenAt'
  {-# INLINE newOpenAt #-}

instance NewOpenAt (DVS.Vector Word8) where
  newOpenAt = newOpenAt'
  {-# INLINE newOpenAt #-}

instance NewOpenAt (DVS.Vector Word16) where
  newOpenAt = newOpenAt'
  {-# INLINE newOpenAt #-}

instance NewOpenAt (DVS.Vector Word32) where
  newOpenAt = newOpenAt'
  {-# INLINE newOpenAt #-}

instance NewOpenAt (DVS.Vector Word64) where
  newOpenAt = newOpenAt'
  {-# INLINE newOpenAt #-}

instance NewOpenAt Word8 where
  newOpenAt = newOpenAt'
  {-# INLINE newOpenAt #-}

instance NewOpenAt Word16 where
  newOpenAt = newOpenAt'
  {-# INLINE newOpenAt #-}

instance NewOpenAt Word32 where
  newOpenAt = newOpenAt'
  {-# INLINE newOpenAt #-}

instance NewOpenAt Word64 where
  newOpenAt = newOpenAt'
  {-# INLINE newOpenAt #-}
