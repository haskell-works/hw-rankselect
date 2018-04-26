{-# LANGUAGE BangPatterns #-}

module App.Commands.Build
  ( cmdBuild
  ) where

import App.Commands.Options.Type
import App.Load
import Control.Lens
import Control.Monad
import Data.List
import Data.Monoid               ((<>))
import Options.Applicative       hiding (columns)
import System.Directory

import qualified App.Commands.Options.Lens             as L
import qualified HaskellWorks.Data.RankSelect.CsPoppy  as CS
import qualified HaskellWorks.Data.RankSelect.Poppy512 as P512

runBuild :: BuildOptions -> IO ()
runBuild opts = do
  case opts ^. L.indexType of
    CsPoppy  -> do
      entries <- listDirectory "data"
      let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
      forM_ files $ \file -> do
        putStrLn $ "Loading cspoppy for " <> file
        CS.CsPoppy !_ !_ !_ <- loadCsPoppy file
        return ()
    Poppy512 -> do
      entries <- listDirectory "data"
      let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
      forM_ files $ \file -> do
        putStrLn $ "Loading cspoppy for " <> file
        P512.Poppy512 !_ !_ <- loadPoppy512 file
        -- let !_ = select1 msbs 1
        return ()

optsBuild :: Parser BuildOptions
optsBuild = BuildOptions
  <$> option auto
      (   long "index-type"
      <>  help "Index type"
      <>  metavar "INDEX_TYPE"
      )

cmdBuild :: Mod CommandFields (IO ())
cmdBuild = command "build"  $ flip info idm $ runBuild <$> optsBuild
