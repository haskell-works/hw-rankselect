{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMaxSpec where

import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.FromBitTextByteString
import           HaskellWorks.Data.Succinct.BalancedParens
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: Ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

vectorSizedBetween :: Int -> Int -> Gen (ShowVector (DVS.Vector Word64))
vectorSizedBetween a b = do
  n   <- choose (a, b)
  xs  <- sequence [ arbitrary | _ <- [1 .. n] ]
  return $ ShowVector (DVS.fromList xs)

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.BalancedParens.RangeMinMaxSpec" $ do
  it "XXX" $ do
    let v = fromBitTextByteString "11101111 10100101 01111110 10110010 10111011 10111011 00011111 11011100" :: DVS.Vector Word64
    let !rmm = mkRangeMinMaxL0 v
    findClose v 61 `shouldBe` findClose rmm 61
  it "findClose should return the same result" $ do
    forAll (vectorSizedBetween 1 512) $ \(ShowVector v) -> do
      let !rmm = mkRangeMinMaxL0 v
      findClose v 0 `shouldBe` findClose rmm 0
  it "findClose should return the same result over all counts" $ do
    forAll (vectorSizedBetween 1 512) $ \(ShowVector v) -> do
      forAll (choose (1, bitLength v)) $ \p -> do
        let !rmm = mkRangeMinMaxL0 v
        findClose v p `shouldBe` findClose rmm p
  it "nextSibling should return the same result" $ do
    forAll (vectorSizedBetween 1 512) $ \(ShowVector v) -> do
      let !rmm = mkRangeMinMaxL0 v
      nextSibling v 0 `shouldBe` nextSibling rmm 0
  it "nextSibling should return the same result over all counts" $ do
    forAll (vectorSizedBetween 1 512) $ \(ShowVector v) -> do
      forAll (choose (1, bitLength v)) $ \p -> do
        let !rmm = mkRangeMinMaxL0 v
        nextSibling v p `shouldBe` nextSibling rmm p
