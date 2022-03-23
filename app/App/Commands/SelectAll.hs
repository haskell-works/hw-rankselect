{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SelectAll
  ( cmdSelectAll
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512
import Options.Applicative                       hiding (columns)
import System.Directory

import qualified App.Commands.Options.Type as Z
import qualified Data.List                 as L

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

runSelectAll :: Z.SelectAllOptions -> IO ()
runSelectAll opts = case opts ^. the @"indexType" of
  Z.CsPoppy -> do
    entries <- getDirectoryContents "data"
    let files = ("data/" ++) <$> (".ib" `L.isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      v :: CsPoppy <- mmapFromForeignRegion file
      forM_ [1..popCount1 v] $ \i -> do
        let !_ = select1 v i
        return ()
      return ()
  Z.Poppy512 -> do
    entries <- getDirectoryContents "data"
    let files = ("data/" ++) <$> (".ib" `L.isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      v :: Poppy512 <- mmapFromForeignRegion file
      forM_ [1..popCount1 v] $ \i -> do
        let !_ = select1 v i
        return ()
      return ()

optsSelectAll :: Parser Z.SelectAllOptions
optsSelectAll = Z.SelectAllOptions
  <$> option auto
      (   long "index-type"
      <>  help "Index type"
      <>  metavar "INDEX_TYPE"
      )

cmdSelectAll :: Mod CommandFields (IO ())
cmdSelectAll = command "select-all"  $ flip info idm $ runSelectAll <$> optsSelectAll
