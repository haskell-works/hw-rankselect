{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving       #-}
{-# LANGUAGE ScopedTypeVariables              #-}

module HaskellWorks.Data.RankSelect.CsPoppy2Spec (spec) where

import GHC.Exts
import Control.Monad
import Control.Monad.IO.Class
import Data.List                                 (isSuffixOf)
import Data.Maybe
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.BasicGen
import HaskellWorks.Data.RankSelect.CsPoppy2
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude hiding (length)
import System.Directory
import System.IO.MMap
import System.IO.Unsafe
import Test.Hspec

import qualified Data.Vector.Storable      as DVS
import qualified HaskellWorks.Hedgehog.Gen as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

corpusFiles :: [FilePath]
corpusFiles = unsafePerformIO $ do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return files
{-# NOINLINE corpusFiles #-}

loadVector64 :: FilePath -> IO (DVS.Vector Word64)
loadVector64 filename = fromForeignRegion <$> mmapFileForeignPtr filename ReadOnly Nothing

newtype ShowVector a = ShowVector a deriving (Eq, BitShow)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.CsPoppy2.Rank1Spec" $ do
  genRank1Select1Spec (undefined :: CsPoppy2)
  describe "rank1 for Vector Word64 is equivalent to rank1 for CsPoppy2" $ do
    it "on one basic block" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 0 8192) (G.word64 R.constantBounded)
      let w = makeCsPoppy2 v
      popCount1 w === popCount1 v
  describe "rank1 for Vector Word64 is equivalent to rank1 for CsPoppy2" $ do
    it "on empty bitvector" $ require $ withTests 1 $ property $ do
      let v = DVS.empty
      let w = makeCsPoppy2 v
      let i = 0
      rank1 w i === rank1 v i
    it "on one basic block" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 1 8) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makeCsPoppy2 v
      rank1 w i === rank1 v i
    it "on two basic blocks" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 9 16) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makeCsPoppy2 v
      rank1 w i === rank1 v i
    it "on three basic blocks" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 17 24) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (length v * 8))
      let w = makeCsPoppy2 v
      rank1 w i === rank1 v i
  describe "select1 for Vector Word64 is equivalent to select1 for CsPoppy2" $ do
    it "on empty bitvector" $ require $ withTests 1 $ property $ do
      let v = DVS.empty
      let w = makeCsPoppy2 v
      let i = 0
      select1 w i === select1 v i
    it "on one full zero basic block" $ require $ withTests 1 $ property $ do
      let v = fromList [0, 0, 0, 0, 0, 0, 0, 0] :: DVS.Vector Word64
      let w = makeCsPoppy2 v
      select1 w 0 === select1 v 0
    it "on one basic block" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 1 8) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makeCsPoppy2 v
      select1 w i === select1 v i
    it "on two basic blocks" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 9 16) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makeCsPoppy2 v
      select1 w i === select1 v i
    it "on three basic blocks" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 17 24) (G.word64 R.constantBounded)
      i <- forAll $ G.word64 (R.linear 0 (popCount1 v))
      let w = makeCsPoppy2 v
      select1 w i === select1 v i
  describe "Rank select over large buffer" $ do
    it "Rank works" $ require $ property $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makeCsPoppy2 cs
      (rank1 ps `map` [1 .. 4096]) === [(x - 1) `div` 2 + 1 | x <- [1 .. 4096]]
    xit "Rank is consistent with pop count" $ require $ property $ do
      v <- forAll $ G.storableVector (R.linear 0 1024) (G.word64 R.constantBounded)
      w <- forAll $ pure $ makeCsPoppy2 v
      rank1 w (bitLength w) === popCount1 w
    it "Select works" $ require $ property $ do
      let cs = fromJust (bitRead (take 4096 (cycle "10"))) :: DVS.Vector Word64
      let ps = makeCsPoppy2 cs
      (select1 ps `map` [1 .. 2048]) === [1, 3 .. 4096]
  describe "Corpus" $ do
    describe "Rank" $ do
      forM_ corpusFiles $ \corpusFile -> do
        xit corpusFile $ do
          v       <- liftIO $ loadVector64 corpusFile
          let rsbs = makeCsPoppy2 v
          let maxPos = fromIntegral $ DVS.length (csPoppy2Bits rsbs)
          require $ property $ do
            r <- forAll $ G.word64 (R.linear 1 maxPos)
            rank1 rsbs r === rank1 v r
    describe "Select" $ do
      forM_ corpusFiles $ \corpusFile -> do
        it corpusFile $ do
          v       <- liftIO $ loadVector64 corpusFile
          let rsbs = makeCsPoppy2 v
          let pc = popCount1 (csPoppy2Bits rsbs)
          require $ property $ do
            s <- forAll $ G.word64 (R.linear 1 pc)
            select1 v s === select1 v s
