{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.SelectAll
  ( cmdSelectAll
  ) where

import Control.Lens
import Control.Monad
import Data.List
import Data.Monoid                               ((<>))
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512
import Options.Applicative                       hiding (columns)
import System.Directory

import qualified App.Commands.Options.Lens as L
import qualified App.Commands.Options.Type as O

runSelectAll :: O.SelectAllOptions -> IO ()
runSelectAll opts = case opts ^. L.indexType of
  O.CsPoppy -> do
    entries <- listDirectory "data"
    let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      v :: CsPoppy <- mmapFromForeignRegion file
      forM_ [1..popCount1 v] $ \i -> do
        let !_ = select1 v i
        return ()
      return ()
  O.Poppy512 -> do
    entries <- listDirectory "data"
    let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      v :: Poppy512 <- mmapFromForeignRegion file
      forM_ [1..popCount1 v] $ \i -> do
        let !_ = select1 v i
        return ()
      return ()

optsSelectAll :: Parser O.SelectAllOptions
optsSelectAll = O.SelectAllOptions
  <$> option auto
      (   long "index-type"
      <>  help "Index type"
      <>  metavar "INDEX_TYPE"
      )

cmdSelectAll :: Mod CommandFields (IO ())
cmdSelectAll = command "select-all"  $ flip info idm $ runSelectAll <$> optsSelectAll
