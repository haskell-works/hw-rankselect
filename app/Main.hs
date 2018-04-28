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
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512
import Options.Applicative
import System.Directory

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

runPoppy512SelectAll :: IO ()
runPoppy512SelectAll = do
  entries <- listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  forM_ files $ \file -> do
    putStrLn $ "Loading cspoppy for " <> file
    v :: Poppy512 <- mmapFromForeignRegion file
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
    v :: CsPoppy <- mmapFromForeignRegion file
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
