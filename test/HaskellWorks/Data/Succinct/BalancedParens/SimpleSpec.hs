{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.BalancedParens.SimpleSpec where

import           Data.Maybe
import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Succinct.BalancedParens
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: Ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.BalancedParens.SimpleSpec" $ do
  describe "For (()(()())) 1101101000" $ do
    let bs = SimpleBalancedParens (91 :: Word64)
    it "Test 1a" $ findClose bs  1 `shouldBe` Just 10
    it "Test 1b" $ findClose bs  2 `shouldBe` Just  3
    it "Test 1b" $ findClose bs  3 `shouldBe` Just  3
    it "Test 1b" $ findClose bs  4 `shouldBe` Just  9
    it "Test 1b" $ findClose bs  5 `shouldBe` Just  6
    it "Test 1b" $ findClose bs  6 `shouldBe` Just  6
    it "Test 1b" $ findClose bs  7 `shouldBe` Just  8
    it "Test 1b" $ findClose bs  8 `shouldBe` Just  8
    it "Test 1b" $ findClose bs  9 `shouldBe` Just  9
    it "Test 1b" $ findClose bs 10 `shouldBe` Just 10
    it "Test 2a" $ findOpen  bs 10 `shouldBe` Just  1
    it "Test 2b" $ findOpen  bs  3 `shouldBe` Just  2
    it "Test 3a" $ enclose   bs  2 `shouldBe` Just  1
    it "Test 3b" $ enclose   bs  7 `shouldBe` Just  4
