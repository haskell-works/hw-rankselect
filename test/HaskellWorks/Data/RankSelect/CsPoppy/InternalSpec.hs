{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module HaskellWorks.Data.RankSelect.CsPoppy.InternalSpec (spec) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.CsPoppy.Internal
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                       hiding (length)
import Test.Hspec

import qualified Data.Vector.Storable as DVS
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

makeCsPoppyBlocksRef :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyBlocksRef v = DVS.constructN (((DVS.length v + 8 - 1) `div` 8) + 1) genBlocks
  where genBlocks :: DVS.Vector Word64 -> Word64
        genBlocks u = let i = DVS.length u in popCount1 (DVS.take 8 (DVS.drop (i * 8) v))

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.CsInterleavedSpec" $ do
  describe "Interleaved Level 1 & 2" $ do
    it "have all its fields isolated" $ requireProperty $ do
      vx <- forAll $ G.word64 R.constantBounded
      va <- forAll $ G.word64 R.constantBounded
      vb <- forAll $ G.word64 R.constantBounded
      vc <- forAll $ G.word64 R.constantBounded
      let actual =  putCsiX vx .
                    putCsiA va .
                    putCsiB vb .
                    putCsiC vc $ CsInterleaved 0
      getCsiX (putCsiX vx actual) === (vx .&. 0xffffffff)
      getCsiA (putCsiA va actual) === (va .&. 0x3ff)
      getCsiB (putCsiB vb actual) === (vb .&. 0x3ff)
      getCsiC (putCsiC vc actual) === (vc .&. 0x3ff)
    it "have all its fields isolated" $ requireProperty $ do
      vx <- forAll $ G.word64 R.constantBounded
      va <- forAll $ G.word64 R.constantBounded
      vb <- forAll $ G.word64 R.constantBounded
      vc <- forAll $ G.word64 R.constantBounded
      getCsiX (putCsiX vx (CsInterleaved 0)) === (vx .&. 0xffffffff)
      getCsiA (putCsiA va (CsInterleaved 0)) === (va .&. 0x3ff)
      getCsiB (putCsiB vb (CsInterleaved 0)) === (vb .&. 0x3ff)
      getCsiC (putCsiC vc (CsInterleaved 0)) === (vc .&. 0x3ff)
  describe "makeCsPoppyBlocks2" $ do
    it "must behave like makeCsPoppyBlocks1" $ requireProperty $ do
      xs <- forAll $ G.list (R.linear 0 1000) (G.word64 R.constantBounded)
      v  <- forAll $ pure $ DVS.fromList xs
      a  <- forAll $ pure $ makeCsPoppyBlocks2   v
      e  <- forAll $ pure $ makeCsPoppyBlocksRef v
      a === e
