{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0Spec
  ( genSelect0UpTo8Spec
  , genSelect0UpTo16Spec
  , genSelect0UpTo32Spec
  , spec
  ) where

import           Data.Maybe
import           Data.Typeable
import qualified Data.Vector                                                as DV
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PopCount.PopCount0
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

genSelect0UpTo8Spec :: forall s. (Typeable s, BitRead s, Select0 s) => s -> Spec
genSelect0UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select0 10010010 over [0..5] should be 023568" $ do
    let bs = fromJust $ bitRead "10010010" :: Word8
    fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]

genSelect0UpTo16Spec :: forall s. (Typeable s, BitRead s, Select0 s) => s -> Spec
genSelect0UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select0 11011010 00 over [0..5]" $
    let bs = fromJust $ bitRead "1101101 000" :: [Bool] in
    fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
  it "select0 11011010 00000000 over [0..5]" $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: Word32
    fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]

genSelect0UpTo32Spec :: forall s. (Typeable s, BitRead s, Select0 s) => s -> Spec
genSelect0UpTo32Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $
    let bs = fromJust $ bitRead "11000001 10000000 01000000" :: s in
    fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.InternalSpec" $ do
  genSelect0UpTo8Spec  (undefined :: Word8)
  genSelect0UpTo8Spec  (undefined :: Word16)
  genSelect0UpTo8Spec  (undefined :: Word32)
  genSelect0UpTo8Spec  (undefined :: Word64)
  genSelect0UpTo16Spec (undefined :: Word16)
  genSelect0UpTo16Spec (undefined :: Word32)
  genSelect0UpTo16Spec (undefined :: Word64)
  genSelect0UpTo32Spec (undefined :: Word32)
  genSelect0UpTo32Spec (undefined :: Word64)
  genSelect0UpTo8Spec  (undefined :: [Bool])
  genSelect0UpTo16Spec (undefined :: [Bool])
  genSelect0UpTo32Spec (undefined :: [Bool])
  genSelect0UpTo8Spec  (undefined :: [Word8])
  genSelect0UpTo16Spec (undefined :: [Word8])
  genSelect0UpTo32Spec (undefined :: [Word8])
  genSelect0UpTo8Spec  (undefined :: [Word16])
  genSelect0UpTo16Spec (undefined :: [Word16])
  genSelect0UpTo32Spec (undefined :: [Word16])
  genSelect0UpTo8Spec  (undefined :: [Word32])
  genSelect0UpTo16Spec (undefined :: [Word32])
  genSelect0UpTo32Spec (undefined :: [Word32])
  genSelect0UpTo8Spec  (undefined :: [Word64])
  genSelect0UpTo16Spec (undefined :: [Word64])
  genSelect0UpTo32Spec (undefined :: [Word64])
  genSelect0UpTo8Spec  (undefined :: DV.Vector Word8)
  genSelect0UpTo16Spec (undefined :: DV.Vector Word8)
  genSelect0UpTo32Spec (undefined :: DV.Vector Word8)
  genSelect0UpTo8Spec  (undefined :: DV.Vector Word16)
  genSelect0UpTo16Spec (undefined :: DV.Vector Word16)
  genSelect0UpTo32Spec (undefined :: DV.Vector Word16)
  genSelect0UpTo8Spec  (undefined :: DV.Vector Word32)
  genSelect0UpTo16Spec (undefined :: DV.Vector Word32)
  genSelect0UpTo32Spec (undefined :: DV.Vector Word32)
  genSelect0UpTo8Spec  (undefined :: DV.Vector Word64)
  genSelect0UpTo16Spec (undefined :: DV.Vector Word64)
  genSelect0UpTo32Spec (undefined :: DV.Vector Word64)
  genSelect0UpTo8Spec  (undefined :: DVS.Vector Word8)
  genSelect0UpTo16Spec (undefined :: DVS.Vector Word8)
  genSelect0UpTo32Spec (undefined :: DVS.Vector Word8)
  genSelect0UpTo8Spec  (undefined :: DVS.Vector Word16)
  genSelect0UpTo16Spec (undefined :: DVS.Vector Word16)
  genSelect0UpTo32Spec (undefined :: DVS.Vector Word16)
  genSelect0UpTo8Spec  (undefined :: DVS.Vector Word32)
  genSelect0UpTo16Spec (undefined :: DVS.Vector Word32)
  genSelect0UpTo32Spec (undefined :: DVS.Vector Word32)
  genSelect0UpTo8Spec  (undefined :: DVS.Vector Word64)
  genSelect0UpTo16Spec (undefined :: DVS.Vector Word64)
  genSelect0UpTo32Spec (undefined :: DVS.Vector Word64)
  describe "For Word8-Word64" $ do
    it "rank0 for Word16 and Word64 should give same answer for bits 0-7" $
      forAll (choose (0, 8)) $ \(i :: Count) (w :: Word8) ->
        rank0 w i == rank0 (fromIntegral w :: Word64) i
    it "rank0 for Word16 and Word64 should give same answer for bits 0-15" $
      forAll (choose (0, 16)) $ \(i :: Count) (w :: Word16) ->
        rank0 w i == rank0 (fromIntegral w :: Word64) i
    it "rank0 for Word32 and Word64 should give same answer for bits 0-31" $
      forAll (choose (0, 32)) $ \(i :: Count) (w :: Word32) ->
        rank0 w i == rank0 (fromIntegral w :: Word64) i
    it "rank0 for Word32 and Word64 should give same answer for bits 32-64" $
      forAll (choose (0, 32)) $ \(i :: Count) (v :: Word32) (w :: Word32) ->
        let v64 = fromIntegral v :: Word64 in
        let w64 = fromIntegral w :: Word64 in
        rank0 v i + popCount0 w == rank0 ((v64 .<. 32) .|. w64) (i + 32)
    it "rank0 and select0 for Word64 form a galois connection" $ property $
      forAll (choose (0, 32)) $ \(i :: Count) (w :: Word32) -> 1 <= i && i <= popCount0 w ==>
        rank0 w (select0 w i) == i && select0 w (rank0 w (fromIntegral i)) <= fromIntegral i
