{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.List
import Foreign
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Select1
import System.Directory
import System.Environment
import System.IO.MMap

import qualified Data.Vector.Storable                   as DVS
import qualified HaskellWorks.Data.RankSelect.CsPoppy   as CS
import qualified HaskellWorks.Data.RankSelect.Poppy512  as P512
import qualified HaskellWorks.Data.RankSelect.Poppy512S as P512S

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

selectStep :: Count
selectStep = 1000

loadVector64 :: FilePath -> IO (DVS.Vector Word64)
loadVector64 filename = fromForeignRegion <$> mmapFileForeignPtr filename ReadOnly Nothing

loadCsPoppy :: FilePath -> IO CS.CsPoppy
loadCsPoppy filename = CS.makeCsPoppy <$> loadVector64 filename

loadPoppy512 :: FilePath -> IO P512.Poppy512
loadPoppy512 filename = P512.makePoppy512 <$> loadVector64 filename

loadPoppy512S :: FilePath -> IO P512S.Poppy512S
loadPoppy512S filename = P512S.makePoppy512S <$> loadVector64 filename

benchCsPoppyBuild :: IO [Benchmark]
benchCsPoppyBuild = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadVector64 filename) $ \bitString -> bgroup filename
          [ bench "CsPoppy Build"  (whnf CS.makeCsPoppy bitString)
          ]

benchCsPoppyRank1 :: IO [Benchmark]
benchCsPoppyRank1 = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadCsPoppy filename) $ \csPoppy -> bgroup filename
          [ bench "CsPoppy Rank1"  (whnf (CS.rank1 csPoppy) 100)
          ]

benchCsPoppySelect1 :: IO [Benchmark]
benchCsPoppySelect1 = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadCsPoppy filename) $ \rsbs -> bgroup filename
          [ bench "CsPoppy Select1"  (whnf (go rsbs 1 (popCount1 rsbs) ((popCount1 rsbs `div` 100) + 1)) 0)
          ]
        go rsbs a z step acc | a <= z  = go rsbs (a + step) z step (select1 rsbs a + acc)
        go _    _ _ _    acc           = acc
        {-# INLINE go #-}

benchPoppy512Build :: IO [Benchmark]
benchPoppy512Build = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadVector64 filename) $ \bitString -> bgroup filename
          [ bench "Poppy512 Build"  (whnf P512.makePoppy512 bitString)
          ]

benchPoppy512Rank1 :: IO [Benchmark]
benchPoppy512Rank1 = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadPoppy512 filename) $ \csPoppy -> bgroup filename
          [ bench "Poppy512 Rank1"  (whnf (CS.rank1 csPoppy) 100)
          ]

benchPoppy512Select1 :: IO [Benchmark]
benchPoppy512Select1 = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadPoppy512 filename) $ \rsbs -> bgroup filename
          [ bench "Poppy512 Select1"  (whnf (go rsbs 1 (popCount1 rsbs) ((popCount1 rsbs `div` 100) + 1)) 0)
          ]
        go rsbs a z step acc | a <= z  = go rsbs (a + step) z step (select1 rsbs a + acc)
        go _    _ _ _    acc           = acc
        {-# INLINE go #-}

benchPoppy512SBuild :: IO [Benchmark]
benchPoppy512SBuild = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadVector64 filename) $ \bitString -> bgroup filename
          [ bench "Poppy512S Build"  (whnf P512S.makePoppy512S bitString)
          ]

benchPoppy512SRank1 :: IO [Benchmark]
benchPoppy512SRank1 = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadPoppy512S filename) $ \csPoppy -> bgroup filename
          [ bench "Poppy512S Rank1"  (whnf (CS.rank1 csPoppy) 100)
          ]

benchPoppy512SSelect1 :: IO [Benchmark]
benchPoppy512SSelect1 = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (loadPoppy512S filename) $ \rsbs -> bgroup filename
          [ bench "Poppy512S Select1"  (whnf (go rsbs 1 (popCount1 rsbs) ((popCount1 rsbs `div` 100) + 1)) 0)
          ]
        go rsbs a z step acc | a <= z  = go rsbs (a + step) z step (select1 rsbs a + acc)
        go _    _ _ _    acc           = acc
        {-# INLINE go #-}

runCsPoppyBuild :: IO ()
runCsPoppyBuild = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  forM_ files $ \file -> do
    msbs <- loadPoppy512S file
    let !_ = select1 msbs 1
    return ()

runBenchmarks :: IO ()
runBenchmarks = (defaultMain =<<) . (concat <$>) $ sequence
  [ benchCsPoppyBuild
  , benchCsPoppyRank1
  , benchCsPoppySelect1
  , benchPoppy512Build
  , benchPoppy512Rank1
  , benchPoppy512Select1
  , benchPoppy512SBuild
  , benchPoppy512SRank1
  , benchPoppy512SSelect1
  ]


main :: IO ()
main = do
  args <- getArgs
  case args of
    []                -> runBenchmarks
    ["load-cspoppy"]  -> runCsPoppyBuild
    _                 -> putStrLn "Invalid arguments"
