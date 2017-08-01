{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.RankSelect.CsPoppySpec (spec) where

import Data.Maybe
import Data.Word
import GHC.Exts
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.BasicGen
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                   hiding (length)
import Test.Hspec
-- import Test.QuickCheck

import qualified Data.Vector.Storable      as DVS
import qualified HaskellWorks.Hedgehog.Gen as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

-- vectorSizedBetween :: Int -> Int -> Gen (ShowVector (DVS.Vector Word64))
-- vectorSizedBetween a b = do
--   n   <- choose (a, b)
--   xs  <- sequence [ arbitrary | _ <- [1 .. n] ]
--   return $ ShowVector (fromList xs)


spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.CsPoppy.Rank1Spec" $ do
  genRank1Select1Spec (undefined :: CsPoppy)
  describe "rank1 for Vector Word64 is equivalent to rank1 for CsPoppy" $ do
    it "on empty bitvector" $ require $ withTests 1 $ property $ do
      let v = DVS.empty
      let w = makeCsPoppy v
      let i = 0
      rank1 v i === rank1 w i
    it "on one basic block" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 1 8) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makeCsPoppy v
      rank1 v i === rank1 w i
    it "on two basic blocks" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 9 16) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makeCsPoppy v
      rank1 v i === rank1 w i
    it "on three basic blocks" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 17 24) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makeCsPoppy v
      rank1 v i === rank1 w i
  describe "select1 for Vector Word64 is equivalent to select1 for CsPoppy" $ do
    it "on empty bitvector" $ require $ withTests 1 $ property $ do
      let v = DVS.empty
      let w = makeCsPoppy v
      let i = 0
      select1 v i === select1 w i
    it "on one full zero basic block" $ require $ withTests 1 $ property $ do
      let v = fromList [0, 0, 0, 0, 0, 0, 0, 0] :: DVS.Vector Word64
      let w = makeCsPoppy v
      select1 v 0 === select1 w 0
    it "on one basic block" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 1 8) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makeCsPoppy v
      select1 v i === select1 w i
    it "on two basic blocks" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 9 16) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makeCsPoppy v
      select1 v i === select1 w i
    it "on three basic blocks" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 17 24) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makeCsPoppy v
      select1 v i === select1 w i
  describe "Rank select over large buffer" $ do
    it "Rank works" $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makeCsPoppy cs
      (rank1 ps `map` [1 .. 4096]) `shouldBe` [(x - 1) `div` 2 + 1 | x <- [1 .. 4096]]
    it "Select works" $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makeCsPoppy cs
      (select1 ps `map` [1 .. 2048]) `shouldBe` [1, 3 .. 4096]
