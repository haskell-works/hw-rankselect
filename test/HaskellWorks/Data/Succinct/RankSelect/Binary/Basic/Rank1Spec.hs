{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1Spec
  ( genRank1UpTo8Spec
  , genRank1UpTo16Spec
  , spec
  ) where

import           Data.Maybe
import           Data.Typeable
import qualified Data.Vector                                                as DV
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

genRank1UpTo8Spec :: forall s. (Typeable s, BitRead s, Rank1 s) => s -> Spec
genRank1UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank1 10010010 over [0..8] should be 011122233" $ do
    let bs = fromJust (bitRead "10010010") :: s
    fmap (rank1 bs) [0..8] `shouldBe` [0, 1, 1, 1, 2, 2, 2, 3, 3]

genRank1UpTo16Spec :: forall s. (Typeable s, BitRead s, Rank1 s) => s -> Spec
genRank1UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank1 11011010 00000000 over [0..9]" $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: s
    fmap (rank1 bs) [0..16] `shouldBe` [0, 1, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
  it "rank1 11011010 10000000 over [0..9]" $ do
    let bs = fromJust $ bitRead "11011010 10000000" :: s
    fmap (rank1 bs) [0..16] `shouldBe` [0, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6]

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.InternalSpec" $ do
  genRank1UpTo8Spec (undefined :: Word8)
  genRank1UpTo8Spec (undefined :: Word16)
  genRank1UpTo8Spec (undefined :: Word32)
  genRank1UpTo8Spec (undefined :: Word64)
  genRank1UpTo8Spec (undefined :: [Word8])
  genRank1UpTo16Spec (undefined :: [Word8])
  genRank1UpTo8Spec (undefined :: [Word16])
  genRank1UpTo16Spec (undefined :: [Word16])
  genRank1UpTo8Spec (undefined :: [Word32])
  genRank1UpTo16Spec (undefined :: [Word32])
  genRank1UpTo8Spec (undefined :: [Word64])
  genRank1UpTo16Spec (undefined :: [Word64])
  genRank1UpTo8Spec (undefined :: DV.Vector Word8)
  genRank1UpTo16Spec (undefined :: DV.Vector Word8)
  genRank1UpTo8Spec (undefined :: DV.Vector Word16)
  genRank1UpTo16Spec (undefined :: DV.Vector Word16)
  genRank1UpTo8Spec (undefined :: DV.Vector Word32)
  genRank1UpTo16Spec (undefined :: DV.Vector Word32)
  genRank1UpTo8Spec (undefined :: DV.Vector Word64)
  genRank1UpTo16Spec (undefined :: DV.Vector Word64)
  genRank1UpTo8Spec (undefined :: DVS.Vector Word8)
  genRank1UpTo16Spec (undefined :: DVS.Vector Word8)
  genRank1UpTo8Spec (undefined :: DVS.Vector Word16)
  genRank1UpTo16Spec (undefined :: DVS.Vector Word16)
  genRank1UpTo8Spec (undefined :: DVS.Vector Word32)
  genRank1UpTo16Spec (undefined :: DVS.Vector Word32)
  genRank1UpTo8Spec (undefined :: DVS.Vector Word64)
  genRank1UpTo16Spec (undefined :: DVS.Vector Word64)
  describe "For Word8-Word64" $ do
    it "rank1 for Word16 and Word64 should give same answer for bits 0-7" $
      forAll (choose (0, 8)) $ \(i :: Count) (w :: Word8) ->
        rank1 w i == rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word16 and Word64 should give same answer for bits 0-15" $
      forAll (choose (0, 16)) $ \(i :: Count) (w :: Word16) ->
        rank1 w i == rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word32 and Word64 should give same answer for bits 0-31" $
      forAll (choose (0, 32)) $ \(i :: Count) (w :: Word32) ->
        rank1 w i == rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word32 and Word64 should give same answer for bits 32-64" $
      forAll (choose (0, 32)) $ \(i :: Count) (v :: Word32) (w :: Word32) ->
        let v64 = fromIntegral v :: Word64 in
        let w64 = fromIntegral w :: Word64 in
        rank1 v i + popCount1 w == rank1 ((v64 .<. 32) .|. w64) (i + 32)
    it "rank1 and select1 for Word64 form a galois connection" $
      forAll (choose (0, 32)) $ \(i :: Count) (w :: Word32) -> 1 <= i && i <= popCount1 w ==>
        rank1 w (select1 w i) == i && select1 w (rank1 w (fromIntegral i)) <= fromIntegral i
