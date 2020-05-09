{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.UnitTest
  ( cmdUnitTest
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Word
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import Options.Applicative

import qualified App.Commands.Options.Type           as Z
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified System.IO                           as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

runUnitTest :: Z.UnitTestOptions -> IO ()
runUnitTest opts = case opts ^. the @"name" of
  "select1 example 1" -> do
    let v :: DVS.Vector Word64 = DVS.fromList
          [    0,   19,   80,  129,  161,  179,  237,  281,  302,  350,  394,  438,  438,  458,  470,  524
          ,  546,  592,  634,  648,  688,  713,  774,  795,  852,  861,  884,  936,  992,  992, 1001, 1036
          , 1068, 1087, 1109, 1145, 1161, 1179, 1182, 1211, 1234, 1295, 1309, 1335, 1359, 1359, 1361, 1383
          ]
    let r = 144
    let !rsbs = makeCsPoppy v
    let !p = select1 v r

    when (select1 rsbs r /= p) $ error "Failed unit test"
  "select1 example 2" -> do
    fileV <- IO.mmapFromForeignRegion "data/example.ib"
    let csPoppy = makeCsPoppy fileV
    let pc = popCount1 (csPoppyBits csPoppy)
    putStrLn $ "PopCount: " <> show pc
    let r = 21

    let !p = select1 fileV r

    when (select1 csPoppy r /= p) $ error "Failed unit test"
  "select1 example 3" -> do
    fileV <- IO.mmapFromForeignRegion "data/example.ib"
    let csPoppy = makeCsPoppy fileV
    let pc = popCount1 (csPoppyBits csPoppy)
    putStrLn $ "PopCount: " <> show pc
    let r = 38

    let !p = select1 fileV r

    when (select1 csPoppy r /= p) $ error "Failed unit test"
  xs -> IO.putStrLn $ "Invalid unit test" <> xs

optsUnitTest :: Parser Z.UnitTestOptions
optsUnitTest = Z.UnitTestOptions
  <$> strOption
      (   long "name"
      <>  help "Unit test name"
      <>  metavar "STRING"
      )

cmdUnitTest :: Mod CommandFields (IO ())
cmdUnitTest = command "unit-test"  $ flip info idm $ runUnitTest <$> optsUnitTest
