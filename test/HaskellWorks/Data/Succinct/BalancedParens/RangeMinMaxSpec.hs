{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMaxSpec where

import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
-- import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShow
-- import           HaskellWorks.Data.Bits.FromBitTextByteString
-- import           HaskellWorks.Data.Succinct.BalancedParens
-- import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax
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

factor :: Int
factor = 16384
{-# INLINE factor #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.BalancedParens.RangeMinMaxSpec" $ do
  it "Skip tests" $ do
    True `shouldBe` True
  -- it "For a simple bit string can find close" $ do
  --   let v = fromBitTextByteString "11101111 10100101 01111110 10110010 10111011 10111011 00011111 11011100" :: DVS.Vector Word64
  --   let !rmm = mkRangeMinMax v
  --   findClose rmm 61 `shouldBe` findClose v 61
  -- it "findClose should return the same result" $ do
  --   forAll (vectorSizedBetween 1 4) $ \(ShowVector v) -> do
  --     let !rmm = mkRangeMinMax v
  --     let len = bitLength v
  --     [findClose rmm i | i <- [1..len]] `shouldBe `[findClose v i | i <- [1..len]]
  -- it "findClose should return the same result over all counts" $ do
  --   forAll (vectorSizedBetween 1 factor) $ \(ShowVector v) -> do
  --     forAll (choose (1, bitLength v)) $ \p -> do
  --       let !rmm = mkRangeMinMax v
  --       findClose rmm p `shouldBe` findClose v p
  -- it "nextSibling should return the same result" $ do
  --   forAll (vectorSizedBetween 1 factor) $ \(ShowVector v) -> do
  --     let !rmm = mkRangeMinMax v
  --     nextSibling rmm 0 `shouldBe` nextSibling v 0
  -- it "nextSibling should return the same result over all counts" $ do
  --   forAll (vectorSizedBetween 1 factor) $ \(ShowVector v) -> do
  --     forAll (choose (1, bitLength v)) $ \p -> do
  --       let !rmm = mkRangeMinMax v
  --       nextSibling rmm p `shouldBe` nextSibling v p
