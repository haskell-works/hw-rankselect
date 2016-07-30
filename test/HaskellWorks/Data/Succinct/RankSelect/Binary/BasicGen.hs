{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.BasicGen
  ( genBinaryRankSelectSpec
  , genBinaryRank0Select0Spec
  , genBinaryRank1Select1Spec
  ) where

import           Data.Typeable
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0Spec   hiding (spec)
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1Spec   hiding (spec)
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0Spec hiding (spec)
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1Spec hiding (spec)
import           Test.Hspec

genBinaryRank1Select1Spec :: forall s. (Typeable s, BitRead s, Rank1 s, Select1 s) => s -> Spec
genBinaryRank1Select1Spec s = describe "For Rank 1" $ do
  genRank1UpTo8Spec     s
  genRank1UpTo16Spec    s
  genSelect1UpTo8Spec   s
  genSelect1UpTo16Spec  s
  genSelect1UpTo32Spec  s

genBinaryRank0Select0Spec :: forall s. (Typeable s, BitRead s, Rank0 s, Select0 s) => s -> Spec
genBinaryRank0Select0Spec s = describe "For Rank 0" $ do
  genRank0UpTo8Spec     s
  genRank0UpTo16Spec    s
  genSelect0UpTo8Spec   s
  genSelect0UpTo16Spec  s
  genSelect0UpTo32Spec  s

genBinaryRankSelectSpec :: forall s. (Typeable s, BitRead s, Rank0 s, Rank1 s, Select0 s, Select1 s) => s -> Spec
genBinaryRankSelectSpec s = describe "Generically" $ do
  genBinaryRank1Select1Spec    s
  genBinaryRank0Select0Spec    s
