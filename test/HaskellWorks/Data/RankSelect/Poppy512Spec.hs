{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.Poppy512Spec (spec) where

import Data.Maybe
import Data.Word
import GHC.Exts
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select0
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.BasicGen
import HaskellWorks.Data.RankSelect.Poppy512
import HaskellWorks.Data.RankSelect.SpecCommon
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                   hiding (length)
import Test.Hspec

import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.RankSelect.Gen as G
import qualified Hedgehog.Gen                     as G
import qualified Hedgehog.Range                   as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.Poppy512.Rank1Spec" $ do
  genRankSelectSpec (undefined :: Poppy512)
  describe "rank1 for Vector Word64 is equivalent to rank1 for Poppy512" $ do
    it "on empty bitvector" $ requireProperty $ do
      let v = DVS.empty
      let w = makePoppy512 v
      let i = 0
      rank1 v i === rank1 w i
    it "on one basic block" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 1 8) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makePoppy512 v
      rank1 v i === rank1 w i
    it "on two basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 9 16) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makePoppy512 v
      rank1 v i === rank1 w i
    it "on three basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 17 24) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makePoppy512 v
      rank1 v i === rank1 w i
  describe "rank0 for Vector Word64 is equivalent to rank0 for Poppy512" $ do
    it "on empty bitvector" $ requireProperty $ do
      let v = DVS.empty
      let w = makePoppy512 v
      let i = 0
      rank0 v i === rank0 w i
    it "on one basic block" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 1 8) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makePoppy512 v
      rank0 v i === rank0 w i
    it "on two basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 9 16) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makePoppy512 v
      rank0 v i === rank0 w i
    it "on three basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 17 24) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makePoppy512 v
      rank0 v i === rank0 w i
  describe "select0 for Vector Word64 is equivalent to select0 for Poppy512" $ do
    it "on empty bitvector" $ requireProperty $ do
      let v = DVS.empty
      let w = makePoppy512 v
      let i = 0
      select0 v i === select0 w i
    it "on one full zero basic block" $ requireProperty $ do
      let v = fromList [0, 0, 0, 0, 0, 0, 0, 0] :: DVS.Vector Word64
      let w = makePoppy512 v
      select0 v 0 === select0 w 0
    it "on one basic block" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 1 8) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (popCount0 v))
      let w = makePoppy512 v
      select0 v i === select0 w i
    it "on two basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 9 16) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (popCount0 v))
      let w = makePoppy512 v
      select0 v i === select0 w i
    it "on three basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 17 24) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (popCount0 v))
      let w = makePoppy512 v
      select0 v i === select0 w i
  describe "select1 for Vector Word64 is equivalent to select1 for Poppy512" $ do
    it "on empty bitvector" $ requireProperty $ do
      let v = DVS.empty
      let w = makePoppy512 v
      let i = 0
      select1 v i === select1 w i
    it "on one full zero basic block" $ requireProperty $ do
      let v = fromList [0, 0, 0, 0, 0, 0, 0, 0] :: DVS.Vector Word64
      let w = makePoppy512 v
      select1 v 0 === select1 w 0
    it "on one basic block" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 1 8) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makePoppy512 v
      select1 v i === select1 w i
    it "on two basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 9 16) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makePoppy512 v
      select1 v i === select1 w i
    it "on three basic blocks" $ requireProperty $ do
      ShowVector v  <- forAll $ G.vector (R.linear 17 24) (G.word64 R.constantBounded)
      i             <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makePoppy512 v
      select1 v i === select1 w i
  describe "Rank select over large buffer" $ do
    it "Rank works" $ requireProperty $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makePoppy512 cs
      (rank1 ps `map` [1 .. 4096]) === [(x - 1) `div` 2 + 1 | x <- [1 .. 4096]]
    it "Select works" $ requireProperty $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makePoppy512 cs
      (select1 ps `map` [1 .. 2048]) === [1, 3 .. 4096]
