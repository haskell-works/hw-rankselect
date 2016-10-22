{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion.Main
import qualified Data.Vector.Storable                                   as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.Broadword
import           HaskellWorks.Data.Bits.FromBitTextByteString
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Succinct.BalancedParens.FindClose
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvRmmVector :: Int -> IO (RangeMinMax (DVS.Vector Word64))
setupEnvRmmVector n = return $ mkRangeMinMax $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvBP2 :: IO Word64
setupEnvBP2 = return $ DVS.head (fromBitTextByteString "10")

setupEnvBP4 :: IO Word64
setupEnvBP4 = return $ DVS.head (fromBitTextByteString "1100")

setupEnvBP8 :: IO Word64
setupEnvBP8 = return $ DVS.head (fromBitTextByteString "11101000")

setupEnvBP16 :: IO Word64
setupEnvBP16 = return $ DVS.head (fromBitTextByteString "11111000 11100000")

setupEnvBP32 :: IO Word64
setupEnvBP32 = return $ DVS.head (fromBitTextByteString "11111000 11101000 11101000 11100000")

setupEnvBP64 :: IO Word64
setupEnvBP64 = return $ DVS.head (fromBitTextByteString "11111000 11101000 11101000 11101000 11101000 11101000 11101000 11100000")

benchRankSelect :: [Benchmark]
benchRankSelect =
  [ env setupEnvBP2 $ \w -> bgroup "FindClose 2-bit"
    [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
    , bench "Naive"         (whnf (findClose (Naive     w)) 1)
    ]
  , env setupEnvBP4 $ \w -> bgroup "FindClose 4-bit"
    [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
    , bench "Naive"         (whnf (findClose (Naive     w)) 1)
    ]
  , env setupEnvBP8 $ \w -> bgroup "FindClose 8-bit"
    [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
    , bench "Naive"         (whnf (findClose (Naive     w)) 1)
    ]
  , env setupEnvBP16 $ \w -> bgroup "FindClose 16-bit"
    [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
    , bench "Naive"         (whnf (findClose (Naive     w)) 1)
    ]
  , env setupEnvBP32 $ \w -> bgroup "FindClose 32-bit"
    [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
    , bench "Naive"         (whnf (findClose (Naive     w)) 1)
    ]
  , env setupEnvBP64 $ \w -> bgroup "FindClose 64-bit"
    [ bench "Broadword"     (whnf (findClose (Broadword w)) 1)
    , bench "Naive"         (whnf (findClose (Naive     w)) 1)
    ]
  , env (setupEnvVector 1000000) $ \bv -> bgroup "RangeMinMax"
    [ bench "findClose"   (nf   (map (findClose bv)) [0, 1000..10000000])
    ]
  , env (setupEnvVector 1000000) $ \bv -> bgroup "Rank"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (rank1 bv)) [0, 1000..10000000])
    ]
  ]

main :: IO ()
main = defaultMain benchRankSelect
