{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.Simple
  ( RangeMinMax2Simple(..)
  , mkRangeMinMax2Simple
  ) where

import qualified Data.Vector.Storable                                           as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.FindCloseN
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpenN
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           HaskellWorks.Data.Succinct.BalancedParens.NewCloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax2.Internal
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1

data RangeMinMax2Simple = RangeMinMax2Simple
  { rangeMinMaxSimpleBP :: !(DVS.Vector Word64)
  }

mkRangeMinMax2Simple :: DVS.Vector Word64 -> RangeMinMax2Simple
mkRangeMinMax2Simple bp = RangeMinMax2Simple
  { rangeMinMaxSimpleBP = bp
  }

instance TestBit RangeMinMax2Simple where
  (.?.) = (.?.) . rangeMinMaxSimpleBP
  {-# INLINE (.?.) #-}

instance Rank1 RangeMinMax2Simple where
  rank1 = rank1 . rangeMinMaxSimpleBP
  {-# INLINE rank1 #-}

instance Rank0 RangeMinMax2Simple where
  rank0 = rank0 . rangeMinMaxSimpleBP
  {-# INLINE rank0 #-}

instance BitLength RangeMinMax2Simple where
  bitLength = bitLength . rangeMinMaxSimpleBP
  {-# INLINE bitLength #-}

instance RangeMinMax2 RangeMinMax2Simple where
  rmmFindCloseDispatch = rmmFindCloseN
  rmmFindCloseN v s p  = if v `newCloseAt` p
    then if s <= 1
      then Just p
      else rmmFindClose v (s - 1) (p + 1)
    else rmmFindClose v (s + 1) (p + 1)
  {-# INLINE rmmFindCloseDispatch #-}
  {-# INLINE rmmFindCloseN        #-}

instance OpenAt RangeMinMax2Simple where
  openAt = openAt . rangeMinMaxSimpleBP
  {-# INLINE openAt #-}

instance CloseAt RangeMinMax2Simple where
  closeAt = closeAt . rangeMinMaxSimpleBP
  {-# INLINE closeAt #-}

instance NewCloseAt RangeMinMax2Simple where
  newCloseAt = newCloseAt . rangeMinMaxSimpleBP
  {-# INLINE newCloseAt #-}

instance FindOpenN RangeMinMax2Simple where
  findOpenN         = findOpenN . rangeMinMaxSimpleBP
  {-# INLINE findOpenN   #-}

instance FindCloseN RangeMinMax2Simple where
  findCloseN v s p  = (+ 1) `fmap` rmmFindClose v (fromIntegral s) (p - 1)
  {-# INLINE findCloseN  #-}
