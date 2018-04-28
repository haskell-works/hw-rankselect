{-# LANGUAGE ScopedTypeVariables #-}

module App.Load where

import HaskellWorks.Data.FromForeignRegion

import qualified HaskellWorks.Data.RankSelect.CsPoppy  as CS
import qualified HaskellWorks.Data.RankSelect.Poppy512 as P512

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

loadCsPoppy :: FilePath -> IO CS.CsPoppy
loadCsPoppy filename = CS.makeCsPoppy <$> mmapFromForeignRegion filename

loadPoppy512 :: FilePath -> IO P512.Poppy512
loadPoppy512 filename = P512.makePoppy512 <$> mmapFromForeignRegion filename
