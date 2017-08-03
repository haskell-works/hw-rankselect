{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module HaskellWorks.Data.RankSelect.CsInterleavedSpec (spec) where

import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.RankSelect.CsInterleaved
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                        hiding (length)
import Test.Hspec

import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.CsInterleavedSpec" $ do
  describe "Interleaved Level 1 & 2" $ do
    it "have all its fields isolated" $ require $ property $ do
      v1a <- forAll $ G.word64 R.constantBounded
      v2a <- forAll $ G.word64 R.constantBounded
      v2b <- forAll $ G.word64 R.constantBounded
      v2c <- forAll $ G.word64 R.constantBounded
      let actual =  put1a v1a .
                    put2a v2a .
                    put2b v2b .
                    put2c v2c $ CsInterleaved 0
      get1a (put1a v1a actual) === (v1a .&. 0xffffffff)
      get2a (put2a v2a actual) === (v2a .&. 0x3ff)
      get2b (put2b v2b actual) === (v2b .&. 0x3ff)
      get2c (put2c v2c actual) === (v2c .&. 0x3ff)
    it "have all its fields isolated" $ require $ property $ do
      v1a <- forAll $ G.word64 R.constantBounded
      v2a <- forAll $ G.word64 R.constantBounded
      v2b <- forAll $ G.word64 R.constantBounded
      v2c <- forAll $ G.word64 R.constantBounded
      get1a (put1a v1a (CsInterleaved 0)) === (v1a .&. 0xffffffff)
      get2a (put2a v2a (CsInterleaved 0)) === (v2a .&. 0x3ff)
      get2b (put2b v2b (CsInterleaved 0)) === (v2b .&. 0x3ff)
      get2c (put2c v2c (CsInterleaved 0)) === (v2c .&. 0x3ff)
