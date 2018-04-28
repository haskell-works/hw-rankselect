{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import App.Commands
import Control.Monad
import Data.List
import Data.Monoid                               ((<>))
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Select1
import Options.Applicative
import System.Directory

import qualified HaskellWorks.Data.RankSelect.CsPoppy  as CS
import qualified HaskellWorks.Data.RankSelect.Poppy512 as P512

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

loadCsPoppy :: FilePath -> IO CS.CsPoppy
loadCsPoppy filename = CS.makeCsPoppy <$> mmapFromForeignRegion filename

loadPoppy512 :: FilePath -> IO P512.Poppy512
loadPoppy512 filename = P512.makePoppy512 <$> mmapFromForeignRegion filename

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
main = join $ execParser (info cmdOpts idm)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     ["cspoppy-load"]        -> runCsPoppyBuild
--     ["cspoppy-select-all"]  -> runCsPoppySelectAll
--     ["poppy512-load"]       -> runPoppy512Build
--     ["poppy512-select-all"] -> runPoppy512SelectAll
--     _                       -> putStrLn "Invalid arguments"
