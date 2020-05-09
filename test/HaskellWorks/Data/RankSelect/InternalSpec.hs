{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module HaskellWorks.Data.RankSelect.InternalSpec (spec) where

import Data.Maybe
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Rank
import HaskellWorks.Data.RankSelect.Base.Select
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                   hiding (length)
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.InternalSpec" $ do
  describe "For [Bool]" $ do
    it "rank True 10010010 over [0..8] should be 011122233" $ requireProperty $ do
      let bs = fromJust $ bitRead "10010010" :: [Bool]
      fmap (rank True bs) [0..8] === [0, 1, 1, 1, 2, 2, 2, 3, 3]
    it "rank True 10010010 over [0..8] should be 001223445" $ requireProperty $ do
      let bs = fromJust $ bitRead "10010010" :: [Bool]
      fmap (rank False bs) [0..8] === [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "select True 10010010 over [0..3] should be 0147" $ requireProperty $ do
      let bs = fromJust $ bitRead "10010010" :: [Bool]
      fmap (select True bs) [0..3] === [0, 1, 4, 7]
    it "select False 10010010 over [0..5] should be 023568" $ requireProperty $ do
      let bs = fromJust $ bitRead "10010010" :: [Bool]
      fmap (select False bs) [0..5] === [0, 2, 3, 5, 6, 8]
    it "Rank and select form a galois connection" $ requireProperty $ do
      bs <- forAll $ G.list (R.linear 0 70) G.bool
      c  <- forAll $ G.word64 (R.linear 0 (popCount1 bs))
      rank True bs (select True bs c) === c
