{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.RankSelect.CsPoppySpec (spec) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Word
import GHC.Exts
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.BasicGen
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512
import HaskellWorks.Data.Take
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                   hiding (length, take)
import Test.Common
import Test.Hspec

import qualified Data.Vector.Storable      as DVS
import qualified HaskellWorks.Hedgehog.Gen as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.CsPoppySpec" $ do
  genRank1Select1Spec (undefined :: CsPoppy)
  describe "rank1 for Vector Word64 is equivalent to rank1 for CsPoppy" $ do
    it "on empty bitvector" $ require $ withTests 1 $ property $ do
      let v = DVS.empty
      Nice w  <- forAll $ pure $ Nice $ makeCsPoppy v
      let i = 0
      rank1 v i === rank1 w i
    it "on one basic block" $ requireProperty $ do
      v       <- forAll $ G.storableVector (R.linear 1 8) (G.word64 R.constantBounded)
      i       <- forAll $ G.word64 (R.linear 0 (length v * 8))
      Nice w  <- forAll $ pure $ Nice $ makeCsPoppy v
      rank1 v i === rank1 w i
    it "on two basic blocks" $ requireProperty $ do
      v <- forAll $ G.storableVector (R.linear 9 16) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (length v * 8))
      Nice w  <- forAll $ pure $ Nice $ makeCsPoppy v
      rank1 v i === rank1 w i
    it "on three basic blocks" $ requireProperty $ do
      v <- forAll $ G.storableVector (R.linear 17 24) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (length v * 8))
      Nice w  <- forAll $ pure $ Nice $ makeCsPoppy v
      rank1 v i === rank1 w i
  describe "select1 for Vector Word64 is equivalent to select1 for CsPoppy" $ do
    it "on empty bitvector" $ require $ withTests 1 $ property $ do
      let v = DVS.empty
      Nice w  <- forAll $ pure $ Nice $ makeCsPoppy v
      let i = 0
      select1 w i === select1 v i
    it "on one full zero basic block" $ require $ withTests 1 $ property $ do
      let v = fromList [0, 0, 0, 0, 0, 0, 0, 0] :: DVS.Vector Word64
      Nice w  <- forAll $ pure $ Nice $ makeCsPoppy v
      select1 w 0 === select1 v 0
    it "on one basic block" $ requireProperty $ do
      v <- forAll $ G.storableVector (R.linear 1 8) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      Nice w  <- forAll $ pure $ Nice $ makeCsPoppy v
      select1 w i === select1 v i
    it "on two basic blocks" $ requireProperty $ do
      v <- forAll $ G.storableVector (R.linear 9 16) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      Nice w  <- forAll $ pure $ Nice $ makeCsPoppy v
      select1 w i === select1 v i
    it "on three basic blocks" $ requireProperty $ do
      v <- forAll $ G.storableVector (R.linear 17 24) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      Nice w  <- forAll $ pure $ Nice $ makeCsPoppy v
      select1 w i === select1 v i
  describe "Rank select over large buffer" $ do
    it "Rank works" $ requireProperty $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makeCsPoppy cs
      (rank1 ps `map` [1 .. 4096]) === [(x - 1) `div` 2 + 1 | x <- [1 .. 4096]]
    it "Rank is consistent with pop count" $ requireProperty $ do
      v         <- forAll $ G.storableVector (R.linear 0 1024) (G.word64 R.constantBounded)
      Nice rsbs <- forAll $ pure $ Nice (makeCsPoppy v)
      rank1 rsbs (bitLength rsbs) === popCount1 rsbs
    it "Select works" $ do
      let v = DVS.fromList [0x1101001000100001, 0x2202002000200002, 0x3303003000300003] :: DVS.Vector Word64
      let pc = popCount1 v
      requireProperty $ do
        Nice csPoppy  <- forAll $ pure $ Nice (makeCsPoppy v)
        s             <- forAll $ G.word64 (R.linear 1 pc)
        select1 csPoppy s === select1 v s
    it "Rank select should match that of un-indexed implementation" $ requireProperty $ do
      !xs   <- forAll $ G.list (R.linear 1 100) (G.word64 (R.linear 0 100))
      !v    <- forAll $ pure (DVS.fromList (scanl (+) 0 xs))
      !vpc  <- forAll $ pure (popCount1 v)
      !r    <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let !rsbs = makeCsPoppy v

      when (vpc > 0 && r > 0) $ do
        annotate $ "v :   " <> show v
        annotate $ "vpc:  " <> show vpc
        annotate $ "r :   " <> show r

        let !p = select1 v r

        select1 rsbs r === p
  describe "Corpus generic" $ do
    describe "Shrunk Rank" $ do
      forM_ corpusFiles $ \corpusFile -> do
        it corpusFile $ do
          fileV <- liftIO $ mmapFromForeignRegion corpusFile
          requireProperty $ do
            let v = DVS.take 768 fileV
            Nice csPoppy  <- forAll $ pure $ Nice $ makeCsPoppy v
            maxPos        <- forAll $ pure $ fromIntegral $ DVS.length (csPoppyBits csPoppy)
            r <- forAll $ G.word64 (R.linear 1 maxPos)
            rank1 csPoppy r === rank1 v r
    describe "Rank" $ do
      forM_ corpusFiles $ \corpusFile -> do
        it corpusFile $ do
          v <- liftIO $ mmapFromForeignRegion corpusFile
          let csPoppy = makeCsPoppy v
          let maxPos = fromIntegral $ DVS.length (csPoppyBits csPoppy)
          requireProperty $ do
            r <- forAll $ G.word64 (R.linear 1 maxPos)
            rank1 csPoppy r === rank1 v r
    describe "Select" $ do
      forM_ corpusFiles $ \corpusFile -> do
        it corpusFile $ do
          fileV <- liftIO $ mmapFromForeignRegion corpusFile
          let csPoppy = makeCsPoppy fileV
          let pc = popCount1 (csPoppyBits csPoppy)
          requireProperty $ do
            _ <- forAll $ pure $ Nice csPoppy
            s <- forAll $ G.word64 (R.linear 1 pc)
            select1 csPoppy s === select1 fileV s
    describe "Select straw man" $ do
      forM_ corpusFiles $ \corpusFile -> do
        it corpusFile $ do
          fileV <- liftIO $ mmapFromForeignRegion corpusFile
          let csPoppy = makeCsPoppy fileV
          let pc = popCount1 (csPoppyBits csPoppy)
          requireProperty $ do
            _ <- forAll $ pure $ corpusFile
            _ <- forAll $ pure $ csPoppyLayerS csPoppy
            s <- forAll $ G.word64 (R.linear 1 pc)
            select1 csPoppy s === select1 fileV s

  describe "Corpus specific" $ do
    describe "data/sample-000.csv.dix" $ do
      it "A select1" $ requireTest $ do
        (va :: CsPoppy) :*: (vr :: Poppy512) <- liftIO $ mmapFromForeignRegion ("data/sample-000.csv.idx")

        actual <- safely $ select1 va 158209

        actual === select1 vr 158209
      it "Check new samples" $ requireTest $ do
        vr :: Poppy512 <- liftIO $ mmapFromForeignRegion ("data/sample-000.csv.idx")

        let v = poppy512Bits vr
        let pcv = popCount1 v
        let samples = genCsSamples pcv v

        forM_ (zip (DVS.toList samples) [1,8193..]) $ \(sample, i) -> do
          select1 vr i === sample
