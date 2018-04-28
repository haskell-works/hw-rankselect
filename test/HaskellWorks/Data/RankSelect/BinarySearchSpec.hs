{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.BinarySearchSpec (spec) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List                                 (isSuffixOf)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import System.IO.Unsafe
import Test.Hspec

import qualified Data.Vector.Storable      as DVS
import qualified HaskellWorks.Hedgehog.Gen as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R
import qualified System.Directory          as IO
import qualified System.IO                 as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.BinarySearchSpec" $ do
  -- xit "moo moo" $ requireProperty $ do
  --   !xs  <- forAll $ G.list (R.linear 1 100) (G.word64 (R.linear 0 100))
  --   !v   <- forAll $ pure (DVS.fromList (scanl (+) 0 xs))
  --   !mi  <- forAll $ G.word64 (R.linear 0  (fromIntegral (DVS.length v - 1)))
  --   !mj  <- forAll $ G.word64 (R.linear mi (fromIntegral (DVS.length v - 1)))
  --   !vpc <- forAll $ pure (popCount1 v)
  --   !r   <- forAll $ G.word64 (R.linear (v !!! fromIntegral mi) (v !!! fromIntegral mj))

  --   if vpc > 0 && r > 0
  --     then do
  --       annotate $ "xs:   " <> show xs
  --       annotate $ "v :   " <> show v
  --       annotate $ "mi:   " <> show mi
  --       annotate $ "vpc:  " <> show vpc
  --       annotate $ "r :   " <> show r

  --       -- safely $ lookupLayerMFrom1 mi r v
  --       lookupLayerMFrom2 mi mj r v === lookupLayerMFrom1 mi r v

  --       -- liftIO $ failOnException 1
  --       -- putStrLn ""

  --       return ()
  --     else return ()
  it "foo foo" $ requireProperty $ do
    !xs   <- forAll $ G.list (R.linear 1 100) (G.word64 (R.linear 0 100))
    !v    <- forAll $ pure (DVS.fromList (scanl (+) 0 xs))
    !vpc  <- forAll $ pure (popCount1 v)
    !r    <- forAll $ G.word64 (R.linear 0 (popCount1 v))
    let !rsbs = makeCsPoppy v

    if vpc > 0 && r > 0
      then do
        annotate $ "v :   " <> show v
        annotate $ "vpc:  " <> show vpc
        annotate $ "r :   " <> show r

        let !p = select1 v r

        select1 rsbs r === p

        return ()
      else return ()

    True === True

  it "example 1" $ requireTest $ do
    !v <- forAll $ pure $ DVS.fromList
      [    0,   19,   80,  129,  161,  179,  237,  281,  302,  350,  394,  438,  438,  458,  470,  524
      ,  546,  592,  634,  648,  688,  713,  774,  795,  852,  861,  884,  936,  992,  992, 1001, 1036
      , 1068, 1087, 1109, 1145, 1161, 1179, 1182, 1211, 1234, 1295, 1309, 1335, 1359, 1359, 1361, 1383
      ]
    !vpc  <- forAll $ pure (popCount1 v)
    !r    <- forAll $ pure 144
    let !rsbs = makeCsPoppy v

    when (vpc > 0 && r > 0) $ do
      let !p = select1 v r

      select1 rsbs r === p

      return ()
