{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Succinct.BalancedParens.Simple
  ( SimpleBalancedParens(..)
  ) where

import           Control.Monad
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Succinct.BalancedParens.CloseAt
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.OpenAt
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           Prelude                                                    as P

newtype SimpleBalancedParens a = SimpleBalancedParens a
  deriving (BalancedParens, FindOpen, FindClose, Enclose, OpenAt, CloseAt, BitLength, BitShow, Eq, Rank0, Rank1, Select0, Select1, TestBit)

instance Functor SimpleBalancedParens where
  fmap f (SimpleBalancedParens a) = SimpleBalancedParens (f a)
  {-# INLINABLE fmap   #-}

instance BitShow a => Show (SimpleBalancedParens a) where
  show = bitShow
