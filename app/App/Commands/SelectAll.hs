{-# LANGUAGE BangPatterns #-}

module App.Commands.SelectAll
  ( cmdSelectAll
  ) where

import App.Commands.Options.Type
import App.Load
import Control.Lens
import Control.Monad
import Data.List
import Data.Monoid                               ((<>))
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Select1
import Options.Applicative                       hiding (columns)
import System.Directory

import qualified App.Commands.Options.Lens as L

runSelectAll :: SelectAllOptions -> IO ()
runSelectAll opts = case opts ^. L.indexType of
  CsPoppy -> do
    entries <- listDirectory "data"
    let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      v <- loadCsPoppy file
      forM_ [1..popCount1 v] $ \i -> do
        let !_ = select1 v i
        return ()
      return ()
  Poppy512 -> do
    entries <- listDirectory "data"
    let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      v <- loadPoppy512 file
      forM_ [1..popCount1 v] $ \i -> do
        let !_ = select1 v i
        return ()
      return ()

optsSelectAll :: Parser SelectAllOptions
optsSelectAll = SelectAllOptions
  <$> option auto
      (   long "index-type"
      <>  help "Index type"
      <>  metavar "INDEX_TYPE"
      )

cmdSelectAll :: Mod CommandFields (IO ())
cmdSelectAll = command "select-all"  $ flip info idm $ runSelectAll <$> optsSelectAll
