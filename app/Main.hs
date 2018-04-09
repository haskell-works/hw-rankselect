{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.List
import Data.Monoid                               ((<>))
import Foreign
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Select1
import System.Directory
import System.Environment
import System.IO.MMap

import qualified Data.Vector.Storable                  as DVS
import qualified HaskellWorks.Data.RankSelect.CsPoppy  as CS
import qualified HaskellWorks.Data.RankSelect.Poppy512 as P512

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

loadVector64 :: FilePath -> IO (DVS.Vector Word64)
loadVector64 filename = fromForeignRegion <$> mmapFileForeignPtr filename ReadOnly Nothing

loadCsPoppy :: FilePath -> IO CS.CsPoppy
loadCsPoppy filename = CS.makeCsPoppy <$> loadVector64 filename

loadPoppy512 :: FilePath -> IO P512.Poppy512
loadPoppy512 filename = P512.makePoppy512 <$> loadVector64 filename

runCsPoppyBuild :: IO ()
runCsPoppyBuild = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  forM_ files $ \file -> do
    putStrLn $ "Loading cspoppy for " <> file
    CS.CsPoppy !_ !_ !_ <- loadCsPoppy file
    return ()

runPoppy512Build :: IO ()
runPoppy512Build = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  forM_ files $ \file -> do
    putStrLn $ "Loading cspoppy for " <> file
    P512.Poppy512 !_ !_ <- loadPoppy512 file
    -- let !_ = select1 msbs 1
    return ()

runPoppy512SelectAll :: IO ()
runPoppy512SelectAll = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  forM_ files $ \file -> do
    putStrLn $ "Loading cspoppy for " <> file
    v <- loadPoppy512 file
    forM_ [1..popCount1 v] $ \i -> do
      let !_ = select1 v i
      return ()
    return ()

runCsPoppySelectAll :: IO ()
runCsPoppySelectAll = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  forM_ files $ \file -> do
    putStrLn $ "Loading cspoppy for " <> file
    v <- loadCsPoppy file
    forM_ [1..popCount1 v] $ \i -> do
      let !_ = select1 v i
      return ()
    return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["cspoppy-load"]        -> runCsPoppyBuild
    ["cspoppy-select-all"]  -> runCsPoppySelectAll
    ["poppy512-load"]       -> runPoppy512Build
    ["poppy512-select-all"] -> runPoppy512SelectAll
    _                       -> putStrLn "Invalid arguments"
