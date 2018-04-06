{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.Poppy512SSpec (spec) where

import Data.Maybe
import Data.Word
import GHC.Exts
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.BasicGen
import HaskellWorks.Data.RankSelect.Poppy512S
import HaskellWorks.Data.RankSelect.SpecCommon
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                   hiding (length)
import Test.Hspec

import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.RankSelect.Gen as G
import qualified Hedgehog.Gen                     as G
import qualified Hedgehog.Range                   as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.Poppy512S.Rank1Spec" $ do
  genRank1Select1Spec (undefined :: Poppy512S)
  describe "rank1 for Vector Word64 is equivalent to rank1 for Poppy512S" $ do
    it "on empty bitvector" $ requireProperty $ do
      let v = DVS.empty
      let w = makePoppy512S v
      let i = 0
      rank1 v i === rank1 w i
    it "on one basic block" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 1 8) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makePoppy512S v
      rank1 v i === rank1 w i
    it "on two basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 9 16) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makePoppy512S v
      rank1 v i === rank1 w i
    it "on three basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 17 24) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makePoppy512S v
      rank1 v i === rank1 w i
  describe "select1 for Vector Word64 is equivalent to select1 for Poppy512S" $ do
    it "on empty bitvector" $ requireProperty $ do
      let v = DVS.empty
      let w = makePoppy512S v
      let i = 0
      select1 v i === select1 w i
    it "on one full zero basic block" $ requireProperty $ do
      let v = fromList [0, 0, 0, 0, 0, 0, 0, 0] :: DVS.Vector Word64
      let w = makePoppy512S v
      select1 v 0 === select1 w 0
    it "on one basic block" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 1 8) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makePoppy512S v
      select1 v i === select1 w i
    it "on two basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 9 16) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makePoppy512S v
      select1 v i === select1 w i
    it "on three basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 17 24) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makePoppy512S v
      select1 v i === select1 w i
  describe "Rank select over large buffer" $ do
    it "Rank works" $ requireProperty $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makePoppy512S cs
      (rank1 ps `map` [1 .. 4096]) === [(x - 1) `div` 2 + 1 | x <- [1 .. 4096]]
    it "Select works" $ requireProperty $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makePoppy512S cs
      (select1 ps `map` [1 .. 2048]) === [1, 3 .. 4096]
