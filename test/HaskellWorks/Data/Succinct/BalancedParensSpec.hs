{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.BalancedParensSpec where

import           Data.Maybe
import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Succinct.BalancedParens
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.BalancedParensSpec" $ do
  describe "For (()(()())) 1101101000" $ do
    let bs = SimpleBalancedParens (91 :: Word64)
    it "Test 1a" $ findClose bs  1 `shouldBe` 10
    it "Test 1b" $ findClose bs  2 `shouldBe`  3
    it "Test 2a" $ findOpen  bs 10 `shouldBe`  1
    it "Test 2b" $ findOpen  bs  3 `shouldBe`  2
    it "Test 3a" $ enclose   bs  2 `shouldBe`  1
    it "Test 3b" $ enclose   bs  7 `shouldBe`  4
  describe "For (()(()())) 1101101000" $ do
    let bs = SimpleBalancedParens (fromJust (bitRead "1101101000") :: [Bool])
    it "Test 1a" $ findClose bs  1 `shouldBe` 10
    it "Test 1b" $ findClose bs  2 `shouldBe`  3
    it "Test 1b" $ findClose bs  3 `shouldBe`  3
    it "Test 1b" $ findClose bs  4 `shouldBe`  9
    it "Test 2a" $ findOpen  bs 10 `shouldBe`  1
    it "Test 2b" $ findOpen  bs  3 `shouldBe`  2
    it "Test 3a" $ enclose   bs  2 `shouldBe`  1
    it "Test 3b" $ enclose   bs  7 `shouldBe`  4
    it "firstChild 1" $ firstChild bs 1 `shouldBe` 2
    it "firstChild 4" $ firstChild bs 4 `shouldBe` 5
    it "nextSibling 2" $ nextSibling bs 2 `shouldBe` 4
    it "nextSibling 5" $ nextSibling bs 5 `shouldBe` 7
    it "parent 2" $ parent bs 2 `shouldBe` 1
    it "parent 5" $ parent bs 5 `shouldBe` 4
    it "depth  1" $ depth bs  1 `shouldBe` 1
    it "depth  2" $ depth bs  2 `shouldBe` 2
    it "depth  3" $ depth bs  3 `shouldBe` 2
    it "depth  4" $ depth bs  4 `shouldBe` 2
    it "depth  5" $ depth bs  5 `shouldBe` 3
    it "depth  6" $ depth bs  6 `shouldBe` 3
    it "depth  7" $ depth bs  7 `shouldBe` 3
    it "depth  8" $ depth bs  8 `shouldBe` 3
    it "depth  9" $ depth bs  9 `shouldBe` 2
    it "depth 10" $ depth bs 10 `shouldBe` 1
    it "subtreeSize  1" $ subtreeSize bs  1 `shouldBe` 5
    it "subtreeSize  2" $ subtreeSize bs  2 `shouldBe` 1
    it "subtreeSize  3" $ subtreeSize bs  3 `shouldBe` 0
    it "subtreeSize  4" $ subtreeSize bs  4 `shouldBe` 3
    it "subtreeSize  5" $ subtreeSize bs  5 `shouldBe` 1
    it "subtreeSize  6" $ subtreeSize bs  6 `shouldBe` 0
    it "subtreeSize  7" $ subtreeSize bs  7 `shouldBe` 1
    it "subtreeSize  8" $ subtreeSize bs  8 `shouldBe` 0
    it "subtreeSize  9" $ subtreeSize bs  9 `shouldBe` 0
    it "subtreeSize 10" $ subtreeSize bs 10 `shouldBe` 0
  describe "For (()(()())) 11011010 00000000 :: DVS.Vector Word8" $ do
    let bs = SimpleBalancedParens (fromJust (bitRead "11011010 00000000") :: DVS.Vector Word8)
    it "Test 1a" $ findClose bs  1 `shouldBe` 10
    it "Test 1b" $ findClose bs  2 `shouldBe`  3
    it "Test 1b" $ findClose bs  3 `shouldBe`  3
    it "Test 1b" $ findClose bs  4 `shouldBe`  9
    it "Test 2a" $ findOpen  bs 10 `shouldBe`  1
    it "Test 2b" $ findOpen  bs  3 `shouldBe`  2
    it "Test 3a" $ enclose   bs  2 `shouldBe`  1
    it "Test 3b" $ enclose   bs  7 `shouldBe`  4
    it "firstChild 1" $ firstChild bs 1 `shouldBe` 2
    it "firstChild 4" $ firstChild bs 4 `shouldBe` 5
    it "nextSibling 2" $ nextSibling bs 2 `shouldBe` 4
    it "nextSibling 5" $ nextSibling bs 5 `shouldBe` 7
    it "parent 2" $ parent bs 2 `shouldBe` 1
    it "parent 5" $ parent bs 5 `shouldBe` 4
    it "depth  1" $ depth bs  1 `shouldBe` 1
    it "depth  2" $ depth bs  2 `shouldBe` 2
    it "depth  3" $ depth bs  3 `shouldBe` 2
    it "depth  4" $ depth bs  4 `shouldBe` 2
    it "depth  5" $ depth bs  5 `shouldBe` 3
    it "depth  6" $ depth bs  6 `shouldBe` 3
    it "depth  7" $ depth bs  7 `shouldBe` 3
    it "depth  8" $ depth bs  8 `shouldBe` 3
    it "depth  9" $ depth bs  9 `shouldBe` 2
    it "depth 10" $ depth bs 10 `shouldBe` 1
    it "subtreeSize  1" $ subtreeSize bs  1 `shouldBe` 5
    it "subtreeSize  2" $ subtreeSize bs  2 `shouldBe` 1
    it "subtreeSize  3" $ subtreeSize bs  3 `shouldBe` 0
    it "subtreeSize  4" $ subtreeSize bs  4 `shouldBe` 3
    it "subtreeSize  5" $ subtreeSize bs  5 `shouldBe` 1
    it "subtreeSize  6" $ subtreeSize bs  6 `shouldBe` 0
    it "subtreeSize  7" $ subtreeSize bs  7 `shouldBe` 1
    it "subtreeSize  8" $ subtreeSize bs  8 `shouldBe` 0
    it "subtreeSize  9" $ subtreeSize bs  9 `shouldBe` 0
    it "subtreeSize 10" $ subtreeSize bs 10 `shouldBe` 0

-- 11011010 00000000, cursorRank = 2
