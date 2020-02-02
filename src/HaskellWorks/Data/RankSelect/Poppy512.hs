{-# OPTIONS_GHC-funbox-strict-fields #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

module HaskellWorks.Data.RankSelect.Poppy512
    ( Poppy512(..)
    , Rank1(..)
    , makePoppy512
    ) where

import Control.DeepSeq
import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.BalancedParens.Enclose
import HaskellWorks.Data.BalancedParens.FindClose
import HaskellWorks.Data.BalancedParens.FindCloseN
import HaskellWorks.Data.BalancedParens.FindOpen
import HaskellWorks.Data.BalancedParens.FindOpenN
import HaskellWorks.Data.BalancedParens.NewCloseAt
import HaskellWorks.Data.BalancedParens.OpenAt
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select0
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Search
import HaskellWorks.Data.Vector.AsVector64
import Prelude                                         hiding (length)

import qualified Data.Vector.Storable as DVS

data Poppy512 = Poppy512
  { poppy512Bits  :: !(DVS.Vector Word64)
  , poppy512Index :: !(DVS.Vector Word64)
  } deriving (Eq, Show, NFData, Generic)

instance FromForeignRegion Poppy512 where
  fromForeignRegion = makePoppy512 . fromForeignRegion

instance PopCount0 Poppy512 where
  popCount0 = popCount0 . poppy512Bits
  {-# INLINE popCount0 #-}

instance PopCount1 Poppy512 where
  popCount1 = popCount1 . poppy512Bits
  {-# INLINE popCount1 #-}

instance AsVector64 Poppy512 where
  asVector64 = asVector64 . poppy512Bits
  {-# INLINE asVector64 #-}

makePoppy512 :: DVS.Vector Word64 -> Poppy512
makePoppy512 v = Poppy512
  { poppy512Bits  = v
  , poppy512Index = DVS.constructN (((DVS.length v + 7) `div` 8) + 1) gen512Index
  }
  where gen512Index :: DVS.Vector Word64 -> Word64
        gen512Index u = let indexN = DVS.length u - 1 in
          if indexN == -1
            then 0
            else popCount1 (DVS.take 8 (DVS.drop (indexN * 8) v)) + DVS.last u

instance BitLength Poppy512 where
  bitLength v = length (poppy512Bits v) * bitLength (poppy512Bits v !!! 0)
  {-# INLINE bitLength #-}

instance TestBit Poppy512 where
  (.?.) = (.?.) . poppy512Bits
  {-# INLINE (.?.) #-}

instance BitRead Poppy512 where
  bitRead = fmap makePoppy512 . bitRead

instance Rank1 Poppy512 where
  rank1 (Poppy512 v i) p =
    (i !!! toPosition (p `div` 512)) + rank1 (DVS.drop ((fromIntegral p `div` 512) * 8) v) (p `mod` 512)

instance Rank0 Poppy512 where
  rank0 (Poppy512 v i) p =
    p `div` 512 * 512 - (i !!! toPosition (p `div` 512)) + rank0 (DVS.drop ((fromIntegral p `div` 512) * 8) v) (p `mod` 512)

instance Select1 Poppy512 where
  select1 (Poppy512 v i) p = toCount q * 512 + select1 (DVS.drop (fromIntegral q * 8) v) (p - s)
    where q = binarySearch (fromIntegral p) wordAt 0 (fromIntegral $ DVS.length i - 1)
          s = (i !!! q) :: Count
          wordAt = (i !!!)

instance Select0 Poppy512 where
  select0 (Poppy512 v i) p = toCount q * 512 + select0 (DVS.drop (fromIntegral q * 8) v) (p - s)
    where q = binarySearch (fromIntegral p) wordAt 0 (fromIntegral $ DVS.length i - 1)
          s = (fromIntegral q * 512 - (i !!! q)) :: Count
          wordAt o = fromIntegral o * 512 - (i !!! o)

instance OpenAt Poppy512 where
  openAt = openAt . poppy512Bits
  {-# INLINE openAt #-}

instance CloseAt Poppy512 where
  closeAt = closeAt . poppy512Bits
  {-# INLINE closeAt #-}

instance FindOpenN Poppy512 where
  findOpenN = findOpenN . poppy512Bits
  {-# INLINE findOpenN    #-}

instance FindCloseN Poppy512 where
  findCloseN = findCloseN . poppy512Bits
  {-# INLINE findCloseN #-}

instance FindOpen Poppy512 where
  findOpen = findOpen . poppy512Bits
  {-# INLINE findOpen #-}

instance FindClose Poppy512 where
  findClose = findClose . poppy512Bits
  {-# INLINE findClose #-}

instance NewCloseAt Poppy512 where
  newCloseAt = newCloseAt . poppy512Bits
  {-# INLINE newCloseAt #-}

instance Enclose Poppy512 where
  enclose = enclose . poppy512Bits
  {-# INLINE enclose #-}

instance BalancedParens Poppy512 where
  firstChild  = firstChild  . poppy512Bits
  nextSibling = nextSibling . poppy512Bits
  parent      = parent      . poppy512Bits
  {-# INLINE firstChild  #-}
  {-# INLINE nextSibling #-}
  {-# INLINE parent      #-}
