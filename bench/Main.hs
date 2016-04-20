{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Criterion.Main
import qualified Data.ByteString                                     as BS
import qualified Data.ByteString.Internal                            as BSI
import qualified Data.Vector.Storable                                as DVS
import           Data.Word
import           Foreign
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic
import           System.IO.MMap

setupEnvBs :: Int -> IO BS.ByteString
setupEnvBs n = return $ BS.pack (take n (cycle [maxBound, 0]))

setupEnvBss :: Int -> Int -> IO [BS.ByteString]
setupEnvBss n k = setupEnvBs n >>= \v -> return (replicate k v)

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvVectors :: Int -> Int -> IO [DVS.Vector Word64]
setupEnvVectors n k = setupEnvVector n >>= \v -> return (replicate k v)

setupEnvJson :: FilePath -> IO BS.ByteString
setupEnvJson filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

benchRankSelect :: [Benchmark]
benchRankSelect =
  [ env (setupEnvVector 1000000) $ \bv -> bgroup "Rank"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (getCount . rank1  bv)) [0, 1000..10000000])
    ]
  ]

main :: IO ()
main = defaultMain benchRankSelect
