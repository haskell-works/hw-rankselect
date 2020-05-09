{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.RankSelect.BitSeqSpec where

import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.BitSeq       ((<|), (><), (|>))
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.RankSelect.BitSeq        as BS
import qualified HaskellWorks.Data.RankSelect.Gen           as G
import qualified HaskellWorks.Data.RankSelect.Internal.List as L
import qualified Hedgehog.Gen                               as G
import qualified Hedgehog.Range                             as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.Internal.BitSeqSpec" $ do
  it "fromWord64s should produce Rmm of the right size" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    BS.size (BS.fromWord64s ws) === fromIntegral (length ws * 64)
  it "fromWord64s should produce Rmm with the right data" $ requireProperty $ do
    wns <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.word64 R.constantBounded
      <*> G.count (R.linear 1 64)

    BS.size (BS.fromPartialWord64s wns) === sum (snd <$> wns)
  it "fromWord64s should produce Rmm with the right data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    BS.toPartialWord64s (BS.fromWord64s ws) === zip ws (repeat 64)
  it "fromPartialWord64s should produce Rmm with the right data" $ requireProperty $ do
    wns <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.word64 R.constantBounded
      <*> G.count (R.linear 1 64)

    BS.toPartialWord64s (BS.fromPartialWord64s wns) === wns
  it "fromBools should produce Rmm with the right data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)

    BS.toPartialWord64s (BS.fromBools (L.toBools ws)) === zip ws (repeat 64)
  it "drop should drop the right amount of data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let ps = BS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (BS.size ps))

    BS.size (BS.drop n ps) === BS.size ps - n
  it "take should take the right amount of data" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let ps = BS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (BS.size ps))

    BS.size (BS.take n ps) === n
  it "splitAt should split at the correct point" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let ps = BS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (BS.size ps))

    let (lt, rt) = BS.splitAt n ps
    BS.size lt === n
    BS.size rt === BS.size ps - n
    BS.toBools lt >< BS.toBools rt === BS.toBools ps
  it "firstChild should choose the first child" $ requireProperty $ do
    ws <- forAll $ G.list (R.linear 0 10) (G.word64 R.constantBounded)
    let ps = BS.fromWord64s ws
    n  <- forAll $ G.count (R.linear 0 (BS.size ps))

    BS.size (BS.drop n ps) === BS.size ps - n
  it "rose tree should be generatable" $ requireProperty $ do
    bs <- forAll $ G.list (R.linear 1 1000) G.bool

    BS.toBools (BS.fromBools bs) === bs
  it "(><) should append" $ requireTest $ do
    bs1       <- forAll $ G.list (R.linear 1 1000) G.bool
    bs2       <- forAll $ G.list (R.linear 1 1000) G.bool
    ps1       <- forAll $ pure $ BS.fromBools bs1
    ps2       <- forAll $ pure $ BS.fromBools bs2

    BS.toBools (ps1 >< ps2) === BS.toBools ps1 >< BS.toBools ps2
  it "(<|) should cons" $ requireTest $ do
    b         <- forAll $ G.bool
    bs        <- forAll $ G.list (R.linear 1 1000) G.bool
    ps        <- forAll $ pure $ BS.fromBools bs

    BS.toBools (b <| ps) === b:BS.toBools ps
  it "(|>) should snoc" $ requireTest $ do
    b         <- forAll $ G.bool
    bs        <- forAll $ G.list (R.linear 1 1000) G.bool
    ps        <- forAll $ pure $ BS.fromBools bs

    BS.toBools (ps |> b) === BS.toBools ps <> [b]
  describe "one bitseq" $ do
    it "select1" $ requireTest $ do
      n         <- forAll $ G.count (R.linear 0 1000)
      bools     <- forAll $ G.list (R.singleton (fromIntegral n)) G.bool
      bs        <- forAll $ pure $ BS.fromBools bools
      i         <- forAll $ G.count (R.linear 0 n)

      select1 bools i === select1 bs i
    it "rank1" $ requireTest $ do
      n         <- forAll $ G.count (R.linear 0 1000)
      bools     <- forAll $ G.list (R.singleton (fromIntegral n)) G.bool
      bs        <- forAll $ pure $ BS.fromBools bools
      i         <- forAll $ G.count (R.linear 0 (popCount1 bools))

      rank1 bools i === rank1 bs i
    it "popCount1" $ requireTest $ do
      bools     <- forAll $ G.list (R.linear 0 1000) G.bool
      bs        <- forAll $ pure $ BS.fromBools bools

      popCount1 bools === popCount1 bs
  describe "concatenated bitseq" $ do
    it "select1" $ requireTest $ do
      n         <- forAll $ G.count (R.linear 0 1000)
      bools1    <- forAll $ G.list (R.singleton (fromIntegral n)) G.bool
      bools2    <- forAll $ G.list (R.singleton (fromIntegral n)) G.bool
      bs1       <- forAll $ pure $ BS.fromBools bools1
      bs2       <- forAll $ pure $ BS.fromBools bools2
      i         <- forAll $ G.count (R.linear 0 n)

      select1 (bools1 <> bools2) i === select1 (bs1 <> bs2) i
    it "rank1" $ requireTest $ do
      n         <- forAll $ G.count (R.linear 0 1000)
      bools1    <- forAll $ G.list (R.singleton (fromIntegral n)) G.bool
      bools2    <- forAll $ G.list (R.singleton (fromIntegral n)) G.bool
      bs1       <- forAll $ pure $ BS.fromBools bools1
      bs2       <- forAll $ pure $ BS.fromBools bools2
      i         <- forAll $ G.count (R.linear 0 (popCount1 (bools1 <> bools2)))

      rank1 (bools1 <> bools2) i === rank1 (bs1 <> bs2) i
    it "popCount1" $ requireTest $ do
      bools1    <- forAll $ G.list (R.linear 0 1000) G.bool
      bools2    <- forAll $ G.list (R.linear 0 1000) G.bool
      bs1       <- forAll $ pure $ BS.fromBools bools1
      bs2       <- forAll $ pure $ BS.fromBools bools2

      popCount1 (bools1 <> bools2) === popCount1 (bs1 <> bs2)
