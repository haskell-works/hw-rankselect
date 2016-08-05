{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.Excess.MinMaxExcess0Spec (spec) where

import qualified Data.Vector.Storable                             as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.Word
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess0
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: Ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.Excess.MinMaxExcess0Spec" $ do
  describe "For Word8" $ do
    it "Excess should be between min excess and max excess" $
      forAll (choose (0, 255 :: Word8)) $ \w ->
        let (minE, e, maxE) = minMaxExcess0 w in
        minE <= e && e <= maxE
    it "minE2 == minE0 `min` (minE1 + e0)" $
      forAll (choose (0, 255 :: Word8)) $ \w0 ->
        forAll (choose (0, 255 :: Word8)) $ \w1 ->
          let w2 = leConcat w0 w1 in
          let (minE0, e0, _) = minMaxExcess0 w0 in
          let (minE1, _ , _) = minMaxExcess0 w1 in
          let (minE2, _ , _) = minMaxExcess0 w2 in
          minE2 == minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0)" $
      forAll (choose (0, 255 :: Word8)) $ \w0 ->
        forAll (choose (0, 255 :: Word8)) $ \w1 ->
          let w2 = leConcat w0 w1 in
          let (_, e0, maxE0) = minMaxExcess0 w0 in
          let (_, _ , maxE1) = minMaxExcess0 w1 in
          let (_, _ , maxE2) = minMaxExcess0 w2 in
          maxE2 == maxE0 `max` (maxE1 + e0)
    it "minE2 == minE0 `min` (minE1 + e0) via vector" $
      forAll (choose (0, 255 :: Word8)) $ \w0 ->
        forAll (choose (0, 255 :: Word8)) $ \w1 ->
          let w2 = DVS.fromList [w0, w1] in
          let (minE0, e0, _) = minMaxExcess0 w0 in
          let (minE1, _ , _) = minMaxExcess0 w1 in
          let (minE2, _ , _) = minMaxExcess0 w2 in
          minE2 == minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0) via vector" $
      forAll (choose (0, 255 :: Word8)) $ \w0 ->
        forAll (choose (0, 255 :: Word8)) $ \w1 ->
          let w2 = DVS.fromList [w0, w1] in
          let (_, e0, maxE0) = minMaxExcess0 w0 in
          let (_, _ , maxE1) = minMaxExcess0 w1 in
          let (_, _ , maxE2) = minMaxExcess0 w2 in
          maxE2 == maxE0 `max` (maxE1 + e0)
  describe "For Word16" $ do
    it "Excess should be between min excess and max excess" $
      forAll (choose (0, 255 :: Word16)) $ \w ->
        let (minE, e, maxE) = minMaxExcess0 w in
        minE <= e && e <= maxE
    it "minE2 == minE0 `min` (minE1 + e0)" $
      forAll (choose (0, 255 :: Word16)) $ \w0 ->
        forAll (choose (0, 255 :: Word16)) $ \w1 ->
          let w2 = leConcat w0 w1 in
          let (minE0, e0, _) = minMaxExcess0 w0 in
          let (minE1, _ , _) = minMaxExcess0 w1 in
          let (minE2, _ , _) = minMaxExcess0 w2 in
          minE2 == minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0)" $
      forAll (choose (0, 255 :: Word16)) $ \w0 ->
        forAll (choose (0, 255 :: Word16)) $ \w1 ->
          let w2 = leConcat w0 w1 in
          let (_, e0, maxE0) = minMaxExcess0 w0 in
          let (_, _ , maxE1) = minMaxExcess0 w1 in
          let (_, _ , maxE2) = minMaxExcess0 w2 in
          maxE2 == maxE0 `max` (maxE1 + e0)
    it "minE2 == minE0 `min` (minE1 + e0) via vector" $
      forAll (choose (0, 255 :: Word16)) $ \w0 ->
        forAll (choose (0, 255 :: Word16)) $ \w1 ->
          let w2 = DVS.fromList [w0, w1] in
          let (minE0, e0, _) = minMaxExcess0 w0 in
          let (minE1, _ , _) = minMaxExcess0 w1 in
          let (minE2, _ , _) = minMaxExcess0 w2 in
          minE2 == minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0) via vector" $
      forAll (choose (0, 255 :: Word16)) $ \w0 ->
        forAll (choose (0, 255 :: Word16)) $ \w1 ->
          let w2 = DVS.fromList [w0, w1] in
          let (_, e0, maxE0) = minMaxExcess0 w0 in
          let (_, _ , maxE1) = minMaxExcess0 w1 in
          let (_, _ , maxE2) = minMaxExcess0 w2 in
          maxE2 == maxE0 `max` (maxE1 + e0)
    describe "For Word32" $ do
      it "Excess should be between min excess and max excess" $
        forAll (choose (0, 255 :: Word32)) $ \w ->
          let (minE, e, maxE) = minMaxExcess0 w in
          minE <= e && e <= maxE
      it "minE2 == minE0 `min` (minE1 + e0)" $
        forAll (choose (0, 255 :: Word32)) $ \w0 ->
          forAll (choose (0, 255 :: Word32)) $ \w1 ->
            let w2 = leConcat w0 w1 in
            let (minE0, e0, _) = minMaxExcess0 w0 in
            let (minE1, _ , _) = minMaxExcess0 w1 in
            let (minE2, _ , _) = minMaxExcess0 w2 in
            minE2 == minE0 `min` (minE1 + e0)
      it "maxE2 == maxE0 `max` (maxE1 + e0)" $
        forAll (choose (0, 255 :: Word32)) $ \w0 ->
          forAll (choose (0, 255 :: Word32)) $ \w1 ->
            let w2 = leConcat w0 w1 in
            let (_, e0, maxE0) = minMaxExcess0 w0 in
            let (_, _ , maxE1) = minMaxExcess0 w1 in
            let (_, _ , maxE2) = minMaxExcess0 w2 in
            maxE2 == maxE0 `max` (maxE1 + e0)
      it "minE2 == minE0 `min` (minE1 + e0) via vector" $
        forAll (choose (0, 255 :: Word32)) $ \w0 ->
          forAll (choose (0, 255 :: Word32)) $ \w1 ->
            let w2 = DVS.fromList [w0, w1] in
            let (minE0, e0, _) = minMaxExcess0 w0 in
            let (minE1, _ , _) = minMaxExcess0 w1 in
            let (minE2, _ , _) = minMaxExcess0 w2 in
            minE2 == minE0 `min` (minE1 + e0)
      it "maxE2 == maxE0 `max` (maxE1 + e0) via vector" $
        forAll (choose (0, 255 :: Word32)) $ \w0 ->
          forAll (choose (0, 255 :: Word32)) $ \w1 ->
            let w2 = DVS.fromList [w0, w1] in
            let (_, e0, maxE0) = minMaxExcess0 w0 in
            let (_, _ , maxE1) = minMaxExcess0 w1 in
            let (_, _ , maxE2) = minMaxExcess0 w2 in
            maxE2 == maxE0 `max` (maxE1 + e0)
  describe "For Word64" $ do
    it "Excess should be between min excess and max excess" $
      forAll (choose (0, 255 :: Word64)) $ \w ->
        let (minE, e, maxE) = minMaxExcess0 w in
        minE <= e && e <= maxE
    it "minE2 == minE0 `min` (minE1 + e0) via vector" $
      forAll (choose (0, 255 :: Word64)) $ \w0 ->
        forAll (choose (0, 255 :: Word64)) $ \w1 ->
          let w2 = DVS.fromList [w0, w1] in
          let (minE0, e0, _) = minMaxExcess0 w0 in
          let (minE1, _ , _) = minMaxExcess0 w1 in
          let (minE2, _ , _) = minMaxExcess0 w2 in
          minE2 == minE0 `min` (minE1 + e0)
    it "maxE2 == maxE0 `max` (maxE1 + e0) via vector" $
      forAll (choose (0, 255 :: Word64)) $ \w0 ->
        forAll (choose (0, 255 :: Word64)) $ \w1 ->
          let w2 = DVS.fromList [w0, w1] in
          let (_, e0, maxE0) = minMaxExcess0 w0 in
          let (_, _ , maxE1) = minMaxExcess0 w1 in
          let (_, _ , maxE2) = minMaxExcess0 w2 in
          maxE2 == maxE0 `max` (maxE1 + e0)
