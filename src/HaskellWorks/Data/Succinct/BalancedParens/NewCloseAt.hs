{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.NewCloseAt
  ( NewCloseAt(..)
  , newCloseAt'
  ) where

import qualified Data.Vector.Storable             as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning

class NewCloseAt v where
  newCloseAt     :: v -> Count -> Bool

newCloseAt' :: TestBit a => a -> Count -> Bool
newCloseAt' v c = not (v .?. toPosition c)
{-# INLINE newCloseAt' #-}

instance (BitLength a, TestBit a) => NewCloseAt (BitShown a) where
  newCloseAt = newCloseAt' . bitShown
  {-# INLINE newCloseAt #-}

instance NewCloseAt [Bool] where
  newCloseAt = newCloseAt'
  {-# INLINE newCloseAt #-}

instance NewCloseAt (DVS.Vector Word8) where
  newCloseAt = newCloseAt'
  {-# INLINE newCloseAt #-}

instance NewCloseAt (DVS.Vector Word16) where
  newCloseAt = newCloseAt'
  {-# INLINE newCloseAt #-}

instance NewCloseAt (DVS.Vector Word32) where
  newCloseAt = newCloseAt'
  {-# INLINE newCloseAt #-}

instance NewCloseAt (DVS.Vector Word64) where
  newCloseAt = newCloseAt'
  {-# INLINE newCloseAt #-}

instance NewCloseAt Word8 where
  newCloseAt = newCloseAt'
  {-# INLINE newCloseAt #-}

instance NewCloseAt Word16 where
  newCloseAt = newCloseAt'
  {-# INLINE newCloseAt #-}

instance NewCloseAt Word32 where
  newCloseAt = newCloseAt'
  {-# INLINE newCloseAt #-}

instance NewCloseAt Word64 where
  newCloseAt = newCloseAt'
  {-# INLINE newCloseAt #-}
