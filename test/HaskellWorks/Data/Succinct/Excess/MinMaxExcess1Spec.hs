{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.Excess.MinMaxExcess1Spec (spec) where

import           Data.Word
import           HaskellWorks.Data.Bits.Word
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.Excess.MinMaxExcess1Spec" $ do
  it "Excess should be between min excess and max excess" $
    forAll (choose (0, 255 :: Word8)) $ \w ->
      let (minE, e, maxE) = minMaxExcess1 w in
      minE <= e && e <= maxE
  it "minE2 == minE0 `min` (minE1 + e0)" $
    forAll (choose (0, 255 :: Word8)) $ \w0 ->
      forAll (choose (0, 255 :: Word8)) $ \w1 ->
        let w2 :: Word16 = leConcat w0 w1 in
        let (minE0, e0, _) = minMaxExcess1 w0 in
        let (minE1, _ , _) = minMaxExcess1 w1 in
        let (minE2, _ , _) = minMaxExcess1 w2 in
        minE2 == minE0 `min` (minE1 + e0)
  it "maxE2 == maxE0 `max` (maxE1 + e0)" $
    forAll (choose (0, 255 :: Word8)) $ \w0 ->
      forAll (choose (0, 255 :: Word8)) $ \w1 ->
        let w2 :: Word16 = leConcat w0 w1 in
        let (_, e0, maxE0) = minMaxExcess1 w0 in
        let (_, _ , maxE1) = minMaxExcess1 w1 in
        let (_, _ , maxE2) = minMaxExcess1 w2 in
        maxE2 == maxE0 `max` (maxE1 + e0)
