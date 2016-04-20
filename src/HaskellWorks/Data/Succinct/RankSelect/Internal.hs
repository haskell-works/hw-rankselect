{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Succinct.RankSelect.Internal
    ( -- * Rank & Select
      Rank(..)
    , Select(..)
    ) where

import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic

class Eq a => Rank v a where
  rank :: a -> v -> Count -> Count

class Eq a => Select v a where
  select :: a -> v -> Count -> Count

instance Rank [Bool] Bool where
  rank a = if a then rank1 else rank0
  {-# INLINABLE rank #-}

instance Select [Bool] Bool where
  select a = if a then select1 else select0
  {-# INLINABLE select #-}
