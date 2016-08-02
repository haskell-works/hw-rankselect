{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.BalancedParens.Internal
  ( BalancedParens(..)
  , closeAt
  , depth
  , openAt
  , subtreeSize
  ) where

import           Control.Monad
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1

class BalancedParens v where
  findOpen :: v -> Count -> Maybe Count
  findClose :: v -> Count -> Maybe Count
  enclose :: v -> Count -> Maybe Count
  firstChild :: v -> Count -> Maybe Count
  nextSibling :: v -> Count -> Maybe Count
  parent :: v -> Count -> Maybe Count

depth :: (BalancedParens v, Rank0 v, Rank1 v) => v -> Count -> Maybe Count
depth v p = (\q -> rank1 v q - rank0 v q) <$> findOpen v p

subtreeSize :: BalancedParens v => v -> Count -> Maybe Count
subtreeSize v p = (\q -> (q - p + 1) `quot` 2) <$> findClose v p

closeAt :: TestBit a => a -> Count -> Bool
closeAt v c = not (v .?. toPosition (c - 1))
{-# INLINABLE closeAt #-}

openAt :: TestBit a => a -> Count -> Bool
openAt v c = v .?. toPosition (c - 1)
{-# INLINABLE openAt #-}

-----

findOpen' :: (BitLength a, TestBit a) => Count -> a -> Count -> Maybe Count
findOpen' c v p = if 0 < p && p <= bitLength v
  then if v `openAt` p
    then if c == 0
      then Just p
      else findOpen' (c - 1) v (p - 1)
    else findOpen' (c + 1) v (p - 1)
  else Nothing
{-# INLINABLE findOpen' #-}

findClose' :: (BitLength a, TestBit a) => Count -> a -> Count -> Maybe Count
findClose' c v p = if 1 < p && p <= bitLength v
  then if v `closeAt` p
    then if c == 0
      then Just p
      else findClose' (c + 1) v (p + 1)
    else findClose' (c - 1) v (p + 1)
  else Nothing
{-# INLINABLE findClose' #-}

instance BalancedParens [Bool] where
  findOpen    v p = if v `openAt`  p then Just p else findOpen'  (Count 0) v (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findClose' (Count 0) v (p + 1)
  enclose         = findOpen' (Count 1)
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p then Nothing else openAt v `mfilter` (findClose v p >>= (\q -> if p /= q then return (q + 1) else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r       else Nothing)
  {-# INLINABLE findOpen    #-}
  {-# INLINABLE findClose   #-}
  {-# INLINABLE enclose     #-}
  {-# INLINABLE firstChild  #-}
  {-# INLINABLE nextSibling #-}
  {-# INLINABLE parent      #-}

instance BalancedParens (DVS.Vector Word8) where
  findOpen    v p = if v `openAt`  p then Just p else findOpen'  (Count 0) v (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findClose' (Count 0) v (p + 1)
  enclose         = findOpen' (Count 1)
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p then Nothing else openAt v `mfilter` (findClose v p >>= (\q -> if p /= q then return (q + 1) else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r       else Nothing)
  {-# INLINABLE findOpen    #-}
  {-# INLINABLE findClose   #-}
  {-# INLINABLE enclose     #-}
  {-# INLINABLE firstChild  #-}
  {-# INLINABLE nextSibling #-}
  {-# INLINABLE parent      #-}

instance BalancedParens (DVS.Vector Word16) where
  findOpen  v p = if v `openAt`  p then Just p else findOpen'  (Count 0) v (p - 1)
  findClose v p = if v `closeAt` p then Just p else findClose' (Count 0) v (p + 1)
  enclose       = findOpen' (Count 1)
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p then Nothing else openAt v `mfilter` (findClose v p >>= (\q -> if p /= q then return (q + 1) else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r       else Nothing)
  {-# INLINABLE findOpen    #-}
  {-# INLINABLE findClose   #-}
  {-# INLINABLE enclose     #-}
  {-# INLINABLE firstChild  #-}
  {-# INLINABLE nextSibling #-}
  {-# INLINABLE parent      #-}

instance BalancedParens (DVS.Vector Word32) where
  findOpen  v p = if v `openAt`  p then Just p else findOpen'  (Count 0) v (p - 1)
  findClose v p = if v `closeAt` p then Just p else findClose' (Count 0) v (p + 1)
  enclose       = findOpen' (Count 1)
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p then Nothing else openAt v `mfilter` (findClose v p >>= (\q -> if p /= q then return (q + 1) else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r       else Nothing)
  {-# INLINABLE findOpen    #-}
  {-# INLINABLE findClose   #-}
  {-# INLINABLE enclose     #-}
  {-# INLINABLE firstChild  #-}
  {-# INLINABLE nextSibling #-}
  {-# INLINABLE parent      #-}

instance BalancedParens (DVS.Vector Word64) where
  findOpen    v p = if v `openAt`  p then Just p else findOpen'  (Count 0) v (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findClose' (Count 0) v (p + 1)
  enclose         = findOpen' (Count 1)
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p then Nothing else openAt v `mfilter` (findClose v p >>= (\q -> if p /= q then return (q + 1) else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r       else Nothing)
  {-# INLINABLE findOpen    #-}
  {-# INLINABLE findClose   #-}
  {-# INLINABLE enclose     #-}
  {-# INLINABLE firstChild  #-}
  {-# INLINABLE nextSibling #-}
  {-# INLINABLE parent      #-}

instance BalancedParens Word8 where
  findOpen    v p = if v `openAt`  p then Just p else findOpen'  (Count 0) v (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findClose' (Count 0) v (p + 1)
  enclose         = findOpen' (Count 1)
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p then Nothing else openAt v `mfilter` (findClose v p >>= (\q -> if p /= q then return (q + 1) else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r       else Nothing)
  {-# INLINABLE findOpen    #-}
  {-# INLINABLE findClose   #-}
  {-# INLINABLE enclose     #-}
  {-# INLINABLE firstChild  #-}
  {-# INLINABLE nextSibling #-}
  {-# INLINABLE parent      #-}

instance BalancedParens Word16 where
  findOpen    v p = if v `openAt`  p then Just p else findOpen'  (Count 0) v (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findClose' (Count 0) v (p + 1)
  enclose         = findOpen' (Count 1)
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p then Nothing else openAt v `mfilter` (findClose v p >>= (\q -> if p /= q then return (q + 1) else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r       else Nothing)
  {-# INLINABLE findOpen    #-}
  {-# INLINABLE findClose   #-}
  {-# INLINABLE enclose     #-}
  {-# INLINABLE firstChild  #-}
  {-# INLINABLE nextSibling #-}
  {-# INLINABLE parent      #-}

instance BalancedParens Word32 where
  findOpen    v p = if v `openAt`  p then Just p else findOpen'  (Count 0) v (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findClose' (Count 0) v (p + 1)
  enclose         = findOpen' (Count 1)
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p then Nothing else openAt v `mfilter` (findClose v p >>= (\q -> if p /= q then return (q + 1) else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r       else Nothing)
  {-# INLINABLE findOpen    #-}
  {-# INLINABLE findClose   #-}
  {-# INLINABLE enclose     #-}
  {-# INLINABLE firstChild  #-}
  {-# INLINABLE nextSibling #-}
  {-# INLINABLE parent      #-}

instance BalancedParens Word64 where
  findOpen    v p = if v `openAt`  p then Just p else findOpen'  (Count 0) v (p - 1)
  findClose   v p = if v `closeAt` p then Just p else findClose' (Count 0) v (p + 1)
  enclose         = findOpen' (Count 1)
  firstChild  v p = if openAt v p && openAt v (p + 1)   then Just (p + 1) else Nothing
  nextSibling v p = if closeAt v p then Nothing else openAt v `mfilter` (findClose v p >>= (\q -> if p /= q then return (q + 1) else Nothing))
  parent      v p = enclose   v p >>= (\r -> if r >= 1 then return r       else Nothing)
  {-# INLINABLE findOpen    #-}
  {-# INLINABLE findClose   #-}
  {-# INLINABLE enclose     #-}
  {-# INLINABLE firstChild  #-}
  {-# INLINABLE nextSibling #-}
  {-# INLINABLE parent      #-}
