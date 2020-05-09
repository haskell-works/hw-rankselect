{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.BinarySearchSpec (spec) where

import Control.Monad
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable as DVS

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.BinarySearchSpec" $ do
  it "example 1" $ requireTest $ do
    !v <- forAll $ pure $ DVS.fromList
      [    0,   19,   80,  129,  161,  179,  237,  281,  302,  350,  394,  438,  438,  458,  470,  524
      ,  546,  592,  634,  648,  688,  713,  774,  795,  852,  861,  884,  936,  992,  992, 1001, 1036
      , 1068, 1087, 1109, 1145, 1161, 1179, 1182, 1211, 1234, 1295, 1309, 1335, 1359, 1359, 1361, 1383
      ]
    !vpc  <- forAll $ pure (popCount1 v)
    !r    <- forAll $ pure 144
    let !rsbs = makeCsPoppy v

    when (vpc > 0 && r > 0) $ do
      let !p = select1 v r

      select1 rsbs r === p

      return ()
