{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.List
import Foreign
import HaskellWorks.Data.FromForeignRegion
import System.Directory
import System.IO.MMap

import qualified Data.Vector.Storable                 as DVS
import qualified HaskellWorks.Data.RankSelect.CsPoppy as CS

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

loadVector64 :: FilePath -> IO (DVS.Vector Word64)
loadVector64 filename = fromForeignRegion <$> mmapFileForeignPtr filename ReadOnly Nothing

loadCsPoppy :: FilePath -> IO CS.CsPoppy
loadCsPoppy filename = CS.makeCsPoppy <$> loadVector64 filename

benchCsPoppy :: IO [Benchmark]
benchCsPoppy = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadCsPoppy filename) $ \csPoppy -> bgroup filename
          [ bench "CsPoppy"  (whnf (CS.rank1 csPoppy) 100)
          ]

main :: IO ()
main = benchCsPoppy >>= defaultMain
