{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Build
  ( cmdBuild
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.List
import Data.Monoid                         ((<>))
import HaskellWorks.Data.FromForeignRegion
import Options.Applicative
import System.Directory

import qualified App.Commands.Options.Type                            as Z
import qualified HaskellWorks.Data.RankSelect.CsPoppy                 as CS
import qualified HaskellWorks.Data.RankSelect.CsPoppy.Internal.Alpha1 as A1
import qualified HaskellWorks.Data.RankSelect.Poppy512                as P512

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Redundant return"    :: String) #-}

runBuild :: Z.BuildOptions -> IO ()
runBuild opts = case opts ^. the @"indexType" of
  Z.CsPoppy  -> do
    entries <- getDirectoryContents "data"
    let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      CS.CsPoppy !_ !_ (A1.CsPoppyIndex !_ !_) <- mmapFromForeignRegion file
      return ()
  Z.Poppy512 -> do
    entries <- getDirectoryContents "data"
    let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      P512.Poppy512 !_ !_ <- mmapFromForeignRegion file
      return ()

optsBuild :: Parser Z.BuildOptions
optsBuild = Z.BuildOptions
  <$> option auto
      (   long "index-type"
      <>  help "Index type"
      <>  metavar "INDEX_TYPE"
      )

cmdBuild :: Mod CommandFields (IO ())
cmdBuild = command "build"  $ flip info idm $ runBuild <$> optsBuild
