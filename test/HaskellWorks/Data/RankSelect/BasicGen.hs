{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.BasicGen
  ( genRankSelectSpec
  , genRank0Select0Spec
  , genRank1Select1Spec
  ) where

import Data.Maybe
import Data.Typeable
import Data.Word
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select0
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

genRank0UpTo8Spec :: forall s. (Typeable s, BitRead s, Rank0 s, Show s) => s -> Spec
genRank0UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank0 10010010 over [0..8] should be 001223445" $ requireProperty $ do
    let bs = fromJust (bitRead "10010010") :: s
    _ <- forAll $ pure bs
    fmap (rank0 bs) [0..8] === [0, 0, 1, 2, 2, 3, 4, 4, 5]

genRank0UpTo16Spec :: forall s. (Typeable s, BitRead s, Rank0 s, Show s) => s -> Spec
genRank0UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank0 11011010 00000000 over [0..16]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: s
    _ <- forAll $ pure bs
    fmap (rank0 bs) [0..16] === [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  it "rank0 11011010 10000000 over [0..16]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 10000000" :: s
    _ <- forAll $ pure bs
    fmap (rank0 bs) [0..16] === [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]

genRank1UpTo8Spec :: forall s. (Typeable s, BitRead s, Rank1 s, Show s) => s -> Spec
genRank1UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank1 10010010 over [0..8] should be 011122233" $ requireProperty $ do
    let bs = fromJust (bitRead "10010010") :: s
    _ <- forAll $ pure bs
    fmap (rank1 bs) [0..8] === [0, 1, 1, 1, 2, 2, 2, 3, 3]

genRank1UpTo16Spec :: forall s. (Typeable s, BitRead s, Rank1 s, Show s) => s -> Spec
genRank1UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "rank1 11011010 00000000 over [0..9]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: s
    _ <- forAll $ pure bs
    fmap (rank1 bs) [0..16] === [0, 1, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
  it "rank1 11011010 10000000 over [0..9]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 10000000" :: s
    _ <- forAll $ pure bs
    fmap (rank1 bs) [0..16] === [0, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6]

genSelect0UpTo8Spec :: forall s. Typeable s => s -> Spec
genSelect0UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select0 10010010 over [0..5] should be 023568" $ requireProperty $ do
    let bs = fromJust $ bitRead "10010010" :: Word8
    _ <- forAll $ pure bs
    fmap (select0 bs) [0..5] === [0, 2, 3, 5, 6, 8]

genSelect0UpTo16Spec :: forall s. Typeable s => s -> Spec
genSelect0UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select0 11011010 00 over [0..5]" $ requireProperty $ do
    let bs = fromJust $ bitRead "1101101 000" :: [Bool]
    _ <- forAll $ pure bs
    fmap (select0 bs) [0..5] === [0, 3, 6, 8, 9, 10]
  it "select0 11011010 00000000 over [0..5]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: Word32
    _ <- forAll $ pure bs
    fmap (select0 bs) [0..11] === [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]

genSelect0UpTo32Spec :: forall s. (Typeable s, BitRead s, Select0 s, Show s) => s -> Spec
genSelect0UpTo32Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ requireProperty $ do
    let bs = fromJust $ bitRead "11000001 10000000 01000000" :: s
    _ <- forAll $ pure bs
    fmap (select0 bs) [0..19] === [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]

genSelect1UpTo8Spec :: forall s. (Typeable s, BitRead s, Select1 s, Show s) => s -> Spec
genSelect1UpTo8Spec _ = describe ("Generically up to 8 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select1 10010010 over [0..3] should be 0147" $ requireProperty $ do
    let bs = fromJust $ bitRead "10010010" :: s
    _ <- forAll $ pure $ bs
    fmap (select1 bs) [0..3] === [0, 1, 4, 7]

genSelect1UpTo16Spec :: forall s. (Typeable s, BitRead s, Select1 s, Show s) => s -> Spec
genSelect1UpTo16Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select1 11011010 00 over [0..5]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 00" :: s
    _ <- forAll $ pure bs
    fmap (select1 bs) [0..5] === [0, 1, 2, 4, 5, 7]
  it "select1 11011010 00000000 over [0..5]" $ requireProperty $ do
    let bs = fromJust $ bitRead "11011010 00000000" :: s
    _ <- forAll $ pure bs
    fmap (select1 bs) [0..5] === [0, 1, 2, 4, 5, 7]
  it "select 01000000 00000100 over [0..2]" $ requireProperty $ do
    let bs = fromJust $ bitRead "01000000 00000100" :: s
    _ <- forAll $ pure bs
    fmap (select1 bs) [0..2] === [0, 2, 14]

genSelect1UpTo32Spec :: forall s. (Typeable s, BitRead s, Select1 s, Show s) => s -> Spec
genSelect1UpTo32Spec _ = describe ("Generically up to 16 bits for " ++ show (typeOf (undefined :: s))) $ do
  it "select1 11000001 10000000 01000000 over [0..5] should be 023568" $ requireProperty $ do
    let bs = fromJust $ bitRead "11000001 10000000 01000000" :: s
    _ <- forAll $ pure bs
    fmap (select1 bs) [0..5] === [0, 1, 2, 8, 9, 18]
  it "select 10000010 00000000 00100000 00010000 over [0..4]" $ requireProperty $ do
    let bs = fromJust $ bitRead "10000010 00000000 00100000 00010000" :: s
    _ <- forAll $ pure bs
    fmap (select1 bs) [0..4] === [0, 1, 7, 19, 28]

genRank1Select1Spec :: forall s. (Typeable s, BitRead s, Rank1 s, Select1 s, Show s) => s -> Spec
genRank1Select1Spec s = describe "For Rank 1" $ do
  genRank1UpTo8Spec     s
  genRank1UpTo16Spec    s
  genSelect1UpTo8Spec   s
  genSelect1UpTo16Spec  s
  genSelect1UpTo32Spec  s

genRank0Select0Spec :: forall s. (Typeable s, BitRead s, Rank0 s, Select0 s, Show s) => s -> Spec
genRank0Select0Spec s = describe "For Rank 0" $ do
  genRank0UpTo8Spec     s
  genRank0UpTo16Spec    s
  genSelect0UpTo8Spec   s
  genSelect0UpTo16Spec  s
  genSelect0UpTo32Spec  s

genRankSelectSpec :: forall s. (Typeable s, BitRead s, Rank0 s, Rank1 s, Select0 s, Select1 s, Show s) => s -> Spec
genRankSelectSpec s = describe "Generically" $ do
  genRank1Select1Spec    s
  genRank0Select0Spec    s
