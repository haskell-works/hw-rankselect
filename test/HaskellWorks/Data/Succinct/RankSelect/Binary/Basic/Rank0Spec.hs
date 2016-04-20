{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0Spec
  ( genRank0UpTo8Spec
  , genRank0UpTo16Spec
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

genRank0UpTo8Spec :: forall s. (Typeable s, BitRead s, Rank0 s) => s -> Spec
genRank0UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank0 10010010 over [0..8] should be 001223445" $ do
    let bs = fromJust (bitRead "10010010") :: s
    fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]

genRank0UpTo16Spec :: forall s. (Typeable s, BitRead s, Rank0 s) => s -> Spec
genRank0UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank0 11011010 00000000 over [0..16]" $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: s
    fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  it "rank0 11011010 10000000 over [0..16]" $ do
    let bs = fromJust $ bitRead "11011010 10000000" :: s
    fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.InternalSpec" $ do
  genRank0UpTo8Spec (undefined :: Word8)
  genRank0UpTo8Spec (undefined :: Word16)
  genRank0UpTo8Spec (undefined :: Word32)
  genRank0UpTo8Spec (undefined :: Word64)
  genRank0UpTo8Spec (undefined :: [Word8])
  genRank0UpTo16Spec (undefined :: [Word8])
  genRank0UpTo8Spec (undefined :: [Word16])
  genRank0UpTo16Spec (undefined :: [Word16])
  genRank0UpTo8Spec (undefined :: [Word32])
  genRank0UpTo16Spec (undefined :: [Word32])
  genRank0UpTo8Spec (undefined :: [Word64])
  genRank0UpTo16Spec (undefined :: [Word64])
  genRank0UpTo8Spec (undefined :: DV.Vector Word8)
  genRank0UpTo16Spec (undefined :: DV.Vector Word8)
  genRank0UpTo8Spec (undefined :: DV.Vector Word16)
  genRank0UpTo16Spec (undefined :: DV.Vector Word16)
  genRank0UpTo8Spec (undefined :: DV.Vector Word32)
  genRank0UpTo16Spec (undefined :: DV.Vector Word32)
  genRank0UpTo8Spec (undefined :: DV.Vector Word64)
  genRank0UpTo16Spec (undefined :: DV.Vector Word64)
  genRank0UpTo8Spec (undefined :: DVS.Vector Word8)
  genRank0UpTo16Spec (undefined :: DVS.Vector Word8)
  genRank0UpTo8Spec (undefined :: DVS.Vector Word16)
  genRank0UpTo16Spec (undefined :: DVS.Vector Word16)
  genRank0UpTo8Spec (undefined :: DVS.Vector Word32)
  genRank0UpTo16Spec (undefined :: DVS.Vector Word32)
  genRank0UpTo8Spec (undefined :: DVS.Vector Word64)
  genRank0UpTo16Spec (undefined :: DVS.Vector Word64)
  describe "Different word sizes give the same rank0" $ do
    it "when comparing Word16 and Word64 over bits 0-7" $
      forAll (choose (0, 8 :: Count)) $ \(i :: Count) (w :: Word8) ->
        rank0 w i == rank0 (fromIntegral w :: Word64) i
    it "when comparing Word16 and Word64 over bits 0-15" $ property $
      forAll (choose (0, 16 :: Count)) $ \(i :: Count) (w :: Word16) ->
        rank0 w i == rank0 (fromIntegral w :: Word64) i
    it "when comparing Word32 and Word64 over bits 0-31" $ property $
      forAll (choose (0, 32 :: Count)) $ \(i :: Count) (w :: Word32) ->
        rank0 w i == rank0 (fromIntegral w :: Word64) i
    it "when comparing Word32 and Word64 over bits 32-64" $ property $
      forAll (choose (0, 32 :: Count)) $ \(i :: Count) (v :: Word32) (w :: Word32) ->
        let v64 = fromIntegral v :: Word64 in
        let w64 = fromIntegral w :: Word64 in
        rank0 v i + popCount0 w == rank0 ((v64 .<. 32) .|. w64) (i + 32)
    it "when comparing select1 for Word64 form a galois connection" $ property $
      forAll (choose (0, 32 :: Count)) $ \(i :: Count) (w :: Word32) ->
        1 <= i && i <= popCount0 w ==>
          rank0 w (select0 w i) == i && select0 w (rank0 w (fromIntegral i)) <= fromIntegral i
