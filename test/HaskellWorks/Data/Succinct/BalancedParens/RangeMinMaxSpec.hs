{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMaxSpec where

import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShow
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
  it "findClose should return the same result" $ do
    forAll (vectorSizedBetween 1 8) $ \(ShowVector v) -> do
      let rmm = mkRangeMinMaxL0 v
      findClose v 0 `shouldBe` findClose rmm 0
  it "nextSibling should return the same result" $ do
    forAll (vectorSizedBetween 1 8) $ \(ShowVector v) -> do
      let rmm = mkRangeMinMaxL0 v
      nextSibling v 0 `shouldBe` nextSibling rmm 0
