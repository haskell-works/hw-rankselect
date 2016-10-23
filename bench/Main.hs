{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion.Main
import qualified Data.Vector.Storable                                   as DVS
import           Data.Word
import           HaskellWorks.Data.RankSelect.Base

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

benchRankSelect :: [Benchmark]
benchRankSelect =
  [ env (setupEnvVector 1000000) $ \bv -> bgroup "Rank"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (rank1 bv)) [0, 1000..10000000])
    ]
  ]

main :: IO ()
main = defaultMain benchRankSelect
