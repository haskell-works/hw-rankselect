{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.FindClose
  ( FindClose(..)
  ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.Broadword
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Broadword
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.FindCloseN

class FindClose v where
  findClose   :: v -> Count -> Maybe Count

instance (FindClose a) => FindClose (BitShown a) where
  findClose = findClose . bitShown
  {-# INLINE findClose #-}

instance FindClose [Bool] where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindClose (DVS.Vector Word8) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindClose (DVS.Vector Word16) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindClose (DVS.Vector Word32) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindClose (DVS.Vector Word64) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindClose Word8 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindClose Word16 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindClose Word32 where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindClose Word64 where
  findClose = findClose . Broadword
  {-# INLINE findClose #-}

instance FindClose (Naive Word64) where
  findClose v p = if v `closeAt` p then Just p else findCloseN v (Count 1) (p + 1)
  {-# INLINE findClose #-}

instance FindClose (Broadword Word64) where
  findClose (Broadword w) p = let x = w .>. (p - 1) in
    case negate (x .&. 1) .&. findCloseW64 x of
      127 -> Nothing
      r   -> let r' = fromIntegral r + p in if r' > 64 then Nothing else Just r'
  {-# INLINE findClose #-}
