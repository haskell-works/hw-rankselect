{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.ValidateSpec (spec) where

import Control.Monad
import Control.Monad.IO.Class
import Data.List                                 (isSuffixOf)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import System.IO.Unsafe
import Test.Hspec

import qualified Data.Vector.Storable      as DVS
import qualified HaskellWorks.Hedgehog.Gen as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R
import qualified System.Directory          as IO

{- HLINT ignore "Reduce duplication"  -}

entries :: [FilePath]
entries = mfilter (".idx" `isSuffixOf`) <$> unsafePerformIO $ IO.getDirectoryContents "data"

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.ValidateSpec" $ do
  it "Validate generated" $ requireProperty $ do
    !v <- forAll $ G.storableVector (R.linear 5 100) (G.word64 R.constantBounded)

    let !popCounts = DVS.fromList (scanl (+) 0 (popCount1 <$> DVS.toList v))
    let !bitIndex = makeCsPoppy v

    forM_ [0 .. fromIntegral (DVS.length v - 1)] $ \i -> do
      let !w = v !!! i
      let !lastPopCount = popCounts !!! i

      forM_ [0 .. 63] $ \pw -> do
        let !r0 = rank1 w  pw      + lastPopCount
        let !r1 = rank1 w (pw + 1) + lastPopCount
        let !p = fromIntegral (i * 64) + pw

        when (r0 == r1) $ do
          let !actualP = select1 bitIndex r0
          when (actualP > p) $ do
            annotate $ "actualP[" <> show actualP <> "] > p[" <> show p <> "]"
            failure

    True === True
  describe "Validate data/*.idx" $
    forM_ entries $ \entry ->
      it entry $ requireTest $ do
        !(v :: DVS.Vector Word64) <- liftIO $ mmapFromForeignRegion ("data/" <> entry)
        let !popCounts = DVS.fromList (scanl (+) 0 (popCount1 <$> DVS.toList v))
        let !bitIndex = makeCsPoppy v

        forM_ [0 .. fromIntegral (DVS.length v - 1)] $ \i -> do
          let !w = v !!! i
          let !lastPopCount = popCounts !!! i

          forM_ [0 .. 63] $ \pw -> do
            let !r0 = rank1 w  pw      + lastPopCount
            let !r1 = rank1 w (pw + 1) + lastPopCount
            let !p = fromIntegral (i * 64) + pw

            when (r0 == r1) $ do
              let !actualP = select1 bitIndex r0
              when (actualP > p) $ do
                annotate $ "select1 " <> show r0 <> " == " <> "actualP[" <> show actualP <> "] > p[" <> show p <> "]"
                failure
