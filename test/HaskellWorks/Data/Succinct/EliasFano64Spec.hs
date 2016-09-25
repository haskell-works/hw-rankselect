{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.EliasFano64Spec (spec) where

import           Data.Word
import           HaskellWorks.Data.Succinct.EliasFano64
import           Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.EliasFano64Spec" $ do
  it "Empty" $
    fromEliasFano64 (toEliasFano64 ([] :: [Word64])) `shouldBe` ([] :: [Word64])
