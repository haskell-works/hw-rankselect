{-# LANGUAGE FlexibleInstances          #-}

module HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
  ( MinMaxExcess(..)
  , MaxExcess
  , MinExcess
  ) where

import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.FixedBitSize

type MinExcess = Int
type MaxExcess = Int
type Excess    = Int

class MinMaxExcess a where
  minMaxExcess :: a -> (MinExcess, Excess, MaxExcess)

instance MinMaxExcess [Bool] where
  minMaxExcess = go 0 0 0
    where go minE maxE e (x:xs)                   = let ne = if x then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne xs
          go minE maxE e _                        = (minE, maxE, e)

instance MinMaxExcess Word8 where
  minMaxExcess = go 0 0 0 0
    where go minE maxE e n w | n < fixedBitSize w = let ne = if w .?. fromIntegral n then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE e _ _                       = (minE, maxE, e)

instance MinMaxExcess Word16 where
  minMaxExcess = go 0 0 0 0
    where go minE maxE e n w | n < fixedBitSize w = let ne = if w .?. fromIntegral n then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE e _ _                      = (minE, maxE, e)

instance MinMaxExcess Word32 where
  minMaxExcess = go 0 0 0 0
    where go minE maxE e n w | n < fixedBitSize w = let ne = if w .?. fromIntegral n then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE e _ _                      = (minE, maxE, e)

instance MinMaxExcess Word64 where
  minMaxExcess = go 0 0 0 0
    where go minE maxE e n w | n < fixedBitSize w = let ne = if w .?. fromIntegral n then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE e _ _                      = (minE, maxE, e)
