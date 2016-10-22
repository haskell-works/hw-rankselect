{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Succinct.BalancedParens.Simple
  ( SimpleBalancedParens(..)
  ) where

import           Control.Monad
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.RankSelect.Base.Rank0
import           HaskellWorks.Data.RankSelect.Base.Rank1
import           HaskellWorks.Data.RankSelect.Base.Select0
import           HaskellWorks.Data.RankSelect.Base.Select1
import           HaskellWorks.Data.Succinct.BalancedParens.BalancedParens
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.Enclose
import           HaskellWorks.Data.Succinct.BalancedParens.FindClose
import           HaskellWorks.Data.Succinct.BalancedParens.FindOpen
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           Prelude                                                    as P

newtype SimpleBalancedParens a = SimpleBalancedParens a
  deriving (BalancedParens, FindOpen, FindClose, Enclose, OpenAt, CloseAt, BitLength, BitShow, Eq, Rank0, Rank1, Select0, Select1, TestBit)

instance Functor SimpleBalancedParens where
  fmap f (SimpleBalancedParens a) = SimpleBalancedParens (f a)
  {-# INLINABLE fmap   #-}

instance BitShow a => Show (SimpleBalancedParens a) where
  show = bitShow
