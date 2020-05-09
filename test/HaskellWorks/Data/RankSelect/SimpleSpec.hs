{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.SimpleSpec (spec) where

import Data.Vector
import Data.Word
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.RankSelect.Gen as G
import qualified Hedgehog.Gen                     as G
import qualified Hedgehog.Range                   as R

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  it "rank1 for BitShown (Vector Word8) and BitShown (Vector Word64) should give same answer" $ require $ withTests 1 $ property $ do
    i <- forAll $ G.count $ R.linear 0 64
    a <- forAll $ G.word8 $ R.constantBounded
    b <- forAll $ G.word8 $ R.constantBounded
    c <- forAll $ G.word8 $ R.constantBounded
    d <- forAll $ G.word8 $ R.constantBounded
    e <- forAll $ G.word8 $ R.constantBounded
    f <- forAll $ G.word8 $ R.constantBounded
    g <- forAll $ G.word8 $ R.constantBounded
    h <- forAll $ G.word8 $ R.constantBounded
    let a64 = fromIntegral a :: Word64
    let b64 = fromIntegral b :: Word64
    let c64 = fromIntegral c :: Word64
    let d64 = fromIntegral d :: Word64
    let e64 = fromIntegral e :: Word64
    let f64 = fromIntegral f :: Word64
    let g64 = fromIntegral g :: Word64
    let h64 = fromIntegral h :: Word64
    let abcdefgh64 = (h64 .<. 56) .|. (g64 .<. 48) .|. (f64 .<. 40) .|. (e64 .<. 32) .|.
                     (d64 .<. 24) .|. (c64 .<. 16) .|. (b64 .<. 8 ) .|.  a64
    let vec16 = BitShown (fromList [a, b, c, d, e, f, g, h] :: Vector Word8 )
    let vec64 = BitShown (fromList [abcdefgh64]             :: Vector Word64)
    rank1 vec16 i === rank1 vec64 i
  it "rank1 for BitShown (Vector Word16) and BitShown (Vector Word64) should give same answer" $ require $ withTests 1 $ property $ do
    i <- forAll $ G.count $ R.linear 0 64
    a <- forAll $ G.word16 $ R.constantBounded
    b <- forAll $ G.word16 $ R.constantBounded
    c <- forAll $ G.word16 $ R.constantBounded
    d <- forAll $ G.word16 $ R.constantBounded
    let a64 = fromIntegral a :: Word64
    let b64 = fromIntegral b :: Word64
    let c64 = fromIntegral c :: Word64
    let d64 = fromIntegral d :: Word64
    let abcd64 = (d64 .<. 48) .|. (c64 .<. 32) .|. (b64 .<. 16) .|. a64
    let vec16 = BitShown (fromList [a, b, c, d] :: Vector Word16)
    let vec64 = BitShown (fromList [abcd64]     :: Vector Word64)
    rank1 vec16 i === rank1 vec64 i
  it "rank1 for BitShown (Vector Word32) and BitShown (Vector Word64) should give same answer" $ require $ withTests 1 $ property $ do
    i <- forAll $ G.count $ R.linear 0 64
    a <- forAll $ G.word32 $ R.constantBounded
    b <- forAll $ G.word32 $ R.constantBounded
    let a64 = fromIntegral a :: Word64
    let b64 = fromIntegral b :: Word64
    let ab64 = (b64 .<. 32) .|. a64
    let vec32 = BitShown (fromList [a, b] :: Vector Word32)
    let vec64 = BitShown (fromList [ab64] :: Vector Word64)
    rank1 vec32 i === rank1 vec64 i
