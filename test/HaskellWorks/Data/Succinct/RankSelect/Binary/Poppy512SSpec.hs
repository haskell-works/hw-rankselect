{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving       #-}
{-# LANGUAGE ScopedTypeVariables              #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512SSpec (spec) where

import           GHC.Exts
import           Data.Maybe
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.IndexedSeq
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.BasicGen
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512S
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

vectorSizedBetween :: Int -> Int -> Gen (ShowVector (DVS.Vector Word64))
vectorSizedBetween a b = do
  n   <- choose (a, b)
  xs  <- sequence [ arbitrary | _ <- [1 .. n] ]
  return $ ShowVector (fromList xs)

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512S.Rank1Spec" $ do
  genBinaryRank1Select1Spec (undefined :: Poppy512S)
  describe "rank1 for Vector Word64 is equivalent to rank1 for Poppy512S" $ do
    it "on empty bitvector" $
      let v = DVS.empty in
      let w = makePoppy512S v in
      let i = 0 in
      rank1 v i === rank1 w i
    it "on one basic block" $
      forAll (vectorSizedBetween 1 8) $ \(ShowVector v) ->
      forAll (choose (0, vLength v * 8)) $ \i ->
      let w = makePoppy512S v in
      rank1 v i === rank1 w i
    it "on two basic blocks" $
      forAll (vectorSizedBetween 9 16) $ \(ShowVector v) ->
      forAll (choose (0, vLength v * 8)) $ \i ->
      let w = makePoppy512S v in
      rank1 v i === rank1 w i
    it "on three basic blocks" $
      forAll (vectorSizedBetween 17 24) $ \(ShowVector v) ->
      forAll (choose (0, vLength v * 8)) $ \i ->
      let w = makePoppy512S v in
      rank1 v i === rank1 w i
  describe "select1 for Vector Word64 is equivalent to select1 for Poppy512S" $ do
    it "on empty bitvector" $
      let v = DVS.empty in
      let w = makePoppy512S v in
      let i = 0 in
      select1 v i === select1 w i
    it "on one full zero basic block" $
      let v = fromList [0, 0, 0, 0, 0, 0, 0, 0] :: DVS.Vector Word64 in
      let w = makePoppy512S v in
      select1 v 0 === select1 w 0
    it "on one basic block" $
      forAll (vectorSizedBetween 1 8) $ \(ShowVector v) ->
      forAll (choose (0, popCount1 v)) $ \i ->
      let w = makePoppy512S v in
      select1 v i === select1 w i
    it "on two basic blocks" $
      forAll (vectorSizedBetween 9 16) $ \(ShowVector v) ->
      forAll (choose (0, popCount1 v)) $ \i ->
      let w = makePoppy512S v in
      select1 v i === select1 w i
    it "on three basic blocks" $
      forAll (vectorSizedBetween 17 24) $ \(ShowVector v) ->
      forAll (choose (0, popCount1 v)) $ \i ->
      let w = makePoppy512S v in
      select1 v i === select1 w i
  describe "Rank select over large buffer" $ do
    it "Rank works" $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makePoppy512S cs
      (rank1 ps `map` [1 .. 4096]) `shouldBe` [(x - 1) `div` 2 + 1 | x <- [1 .. 4096]]
    it "Select works" $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makePoppy512S cs
      (select1 ps `map` [1 .. 2048]) `shouldBe` [1, 3 .. 4096]
