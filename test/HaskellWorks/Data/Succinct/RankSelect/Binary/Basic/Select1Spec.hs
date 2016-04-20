{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1Spec
  ( genSelect1UpTo8Spec
  , genSelect1UpTo16Spec
  , genSelect1UpTo32Spec
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

genSelect1UpTo8Spec :: forall s. (Typeable s, BitRead s, Select1 s) => s -> Spec
genSelect1UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select1 10010010 over [0..3] should be 0147" $ do
    let bs = fromJust $ bitRead "10010010" :: s
    fmap (select1 bs) [0..3] `shouldBe` [0, 1, 4, 7]

genSelect1UpTo16Spec :: forall s. (Typeable s, BitRead s, Select1 s) => s -> Spec
genSelect1UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select1 11011010 00 over [0..5]" $
    let bs = fromJust $ bitRead "11011010 00" :: s in
    fmap (select1 bs) [0..5] `shouldBe` [0, 1, 2, 4, 5, 7]
  it "select1 11011010 00000000 over [0..5]" $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: s
    fmap (select1 bs) [0..5] `shouldBe` [0, 1, 2, 4, 5, 7]
  it "select 01000000 00000100 over [0..2]" $ do
    let bs = fromJust $ bitRead "01000000 00000100" :: s
    fmap (select1 bs) [0..2] `shouldBe` [0, 2, 14]

genSelect1UpTo32Spec :: forall s. (Typeable s, BitRead s, Select1 s) => s -> Spec
genSelect1UpTo32Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select1 11000001 10000000 01000000 over [0..5] should be 023568" $
    let bs = fromJust $ bitRead "11000001 10000000 01000000" :: s in
    fmap (select1 bs) [0..5] `shouldBe` [0, 1, 2, 8, 9, 18]
  it "select 10000010 00000000 00100000 00010000 over [0..4]" $ do
    let bs = fromJust $ bitRead "10000010 00000000 00100000 00010000" :: s
    fmap (select1 bs) [0..4] `shouldBe` [0, 1, 7, 19, 28]

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.InternalSpec" $ do
  genSelect1UpTo8Spec  (undefined :: Word8)
  genSelect1UpTo8Spec  (undefined :: Word16)
  genSelect1UpTo8Spec  (undefined :: Word32)
  genSelect1UpTo8Spec  (undefined :: Word64)
  genSelect1UpTo16Spec (undefined :: Word16)
  genSelect1UpTo16Spec (undefined :: Word32)
  genSelect1UpTo16Spec (undefined :: Word64)
  genSelect1UpTo32Spec (undefined :: Word32)
  genSelect1UpTo32Spec (undefined :: Word64)
  genSelect1UpTo8Spec  (undefined :: [Bool])
  genSelect1UpTo16Spec (undefined :: [Bool])
  genSelect1UpTo32Spec (undefined :: [Bool])
  genSelect1UpTo8Spec  (undefined :: [Word8])
  genSelect1UpTo16Spec (undefined :: [Word8])
  genSelect1UpTo32Spec (undefined :: [Word8])
  genSelect1UpTo8Spec  (undefined :: [Word16])
  genSelect1UpTo16Spec (undefined :: [Word16])
  genSelect1UpTo32Spec (undefined :: [Word16])
  genSelect1UpTo8Spec  (undefined :: [Word32])
  genSelect1UpTo16Spec (undefined :: [Word32])
  genSelect1UpTo32Spec (undefined :: [Word32])
  genSelect1UpTo8Spec  (undefined :: [Word64])
  genSelect1UpTo16Spec (undefined :: [Word64])
  genSelect1UpTo32Spec (undefined :: [Word64])
  genSelect1UpTo8Spec  (undefined :: DV.Vector Word8)
  genSelect1UpTo16Spec (undefined :: DV.Vector Word8)
  genSelect1UpTo32Spec (undefined :: DV.Vector Word8)
  genSelect1UpTo8Spec  (undefined :: DV.Vector Word16)
  genSelect1UpTo16Spec (undefined :: DV.Vector Word16)
  genSelect1UpTo32Spec (undefined :: DV.Vector Word16)
  genSelect1UpTo8Spec  (undefined :: DV.Vector Word32)
  genSelect1UpTo16Spec (undefined :: DV.Vector Word32)
  genSelect1UpTo32Spec (undefined :: DV.Vector Word32)
  genSelect1UpTo8Spec  (undefined :: DV.Vector Word64)
  genSelect1UpTo16Spec (undefined :: DV.Vector Word64)
  genSelect1UpTo32Spec (undefined :: DV.Vector Word64)
  genSelect1UpTo8Spec  (undefined :: DVS.Vector Word8)
  genSelect1UpTo16Spec (undefined :: DVS.Vector Word8)
  genSelect1UpTo32Spec (undefined :: DVS.Vector Word8)
  genSelect1UpTo8Spec  (undefined :: DVS.Vector Word16)
  genSelect1UpTo16Spec (undefined :: DVS.Vector Word16)
  genSelect1UpTo32Spec (undefined :: DVS.Vector Word16)
  genSelect1UpTo8Spec  (undefined :: DVS.Vector Word32)
  genSelect1UpTo16Spec (undefined :: DVS.Vector Word32)
  genSelect1UpTo32Spec (undefined :: DVS.Vector Word32)
  genSelect1UpTo8Spec  (undefined :: DVS.Vector Word64)
  genSelect1UpTo16Spec (undefined :: DVS.Vector Word64)
  genSelect1UpTo32Spec (undefined :: DVS.Vector Word64)
  describe "For Word64" $ do
    it "rank1 for Word16 and Word64 should give same answer for bits 0-7" $ property $
      forAll (choose (0, 8)) $ \(i :: Count) (w :: Word8) ->
        rank1 w i == rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word16 and Word64 should give same answer for bits 0-15" $ property $
      forAll (choose (0, 16)) $ \(i :: Count) (w :: Word16) ->
        rank1 w i == rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word32 and Word64 should give same answer for bits 0-31" $ property $
      forAll (choose (0, 32)) $ \(i :: Count) (w :: Word32) ->
        rank1 w i == rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word32 and Word64 should give same answer for bits 32-64" $ property $
      forAll (choose (0, 32)) $ \(i :: Count) (v :: Word32) (w :: Word32) ->
        let v64 = fromIntegral v :: Word64 in
        let w64 = fromIntegral w :: Word64 in
        rank1 v i + popCount1 w == rank1 ((v64 .<. 32) .|. w64) (i + 32)
    it "rank1 and select1 for Word64 form a galois connection" $ property $
      forAll (choose (0, 32)) $ \(i :: Count) (w :: Word32) -> 1 <= i && i <= popCount1 w ==>
        rank1 w (select1 w i) == i && select1 w (rank1 w (fromIntegral i)) <= fromIntegral i
