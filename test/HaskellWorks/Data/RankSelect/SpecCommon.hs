{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.RankSelect.SpecCommon where

import HaskellWorks.Data.Bits.BitShow

newtype ShowVector a = ShowVector a deriving (Eq, BitShow)

instance BitShow a => Show (ShowVector a) where
  show = bitShow
