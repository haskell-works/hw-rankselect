{-# LANGUAGE ScopedTypeVariables #-}

module App.Load where

import Foreign
import HaskellWorks.Data.FromForeignRegion
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
