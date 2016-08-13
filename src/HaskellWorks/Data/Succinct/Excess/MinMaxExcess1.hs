{-# LANGUAGE FlexibleInstances          #-}

module HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
  ( MinMaxExcess1(..)
  , MaxExcess
  , MinExcess
  ) where

import qualified Data.Vector.Storable                 as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.FixedBitSize
import           HaskellWorks.Data.Naive

type MinExcess = Int
type MaxExcess = Int
type Excess    = Int

class MinMaxExcess1 a where
  minMaxExcess1 :: a -> (MinExcess, Excess, MaxExcess)

instance MinMaxExcess1 [Bool] where
  minMaxExcess1 = go 0 0 0
    where go minE maxE e (x:xs)                   = let ne = if x then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne xs
          go minE maxE e _                        = (minE, e, maxE)
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (Naive Word8) where
  minMaxExcess1 (Naive w) = go 0 0 0 0 w
    where go minE maxE e n w | n < fixedBitSize w = let ne = if w .?. fromIntegral n then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE e _ _                       = (minE, e, maxE)
  {-# INLINE minMaxExcess1 #-}


instance MinMaxExcess1 Word8 where
  minMaxExcess1 w = ( word8ExcessMin DVS.! fromIntegral w
                    , word8Excess    DVS.! fromIntegral w
                    , word8ExcessMax DVS.! fromIntegral w
                    )
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 Word16 where
  minMaxExcess1 = go 0 0 0 0
    where go minE maxE e n w | n < fixedBitSize w = let ne = if w .?. fromIntegral n then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE e _ _                      = (minE, e, maxE)
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 Word32 where
  minMaxExcess1 = go 0 0 0 0
    where go minE maxE e n w | n < fixedBitSize w = let ne = if w .?. fromIntegral n then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE e _ _                      = (minE, e, maxE)
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 Word64 where
  minMaxExcess1 = go 0 0 0 0
    where go minE maxE e n w | n < fixedBitSize w = let ne = if w .?. fromIntegral n then e + 1 else e - 1 in
                                                        go (minE `min` ne) (maxE `max` ne) ne (n + 1) w
          go minE maxE e _ _                      = (minE, e, maxE)
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (DVS.Vector Word8) where
  minMaxExcess1 = DVS.foldl gen (0, 0, 0)
    where gen :: (MinExcess, Excess, MaxExcess) -> Word8 -> (MinExcess, Excess, MaxExcess)
          gen (minE, e, maxE) w = let (wMinE, wE, wMaxE) = minMaxExcess1 w  in
                                  (minE `min` (wMinE + e), e + wE, maxE `max` (wMaxE + e))
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (DVS.Vector Word16) where
  minMaxExcess1 = DVS.foldl gen (0, 0, 0)
    where gen :: (MinExcess, Excess, MaxExcess) -> Word16 -> (MinExcess, Excess, MaxExcess)
          gen (minE, e, maxE) w = let (wMinE, wE, wMaxE) = minMaxExcess1 w  in
                                  (minE `min` (wMinE + e), e + wE, maxE `max` (wMaxE + e))
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (DVS.Vector Word32) where
  minMaxExcess1 = DVS.foldl gen (0, 0, 0)
    where gen :: (MinExcess, Excess, MaxExcess) -> Word32 -> (MinExcess, Excess, MaxExcess)
          gen (minE, e, maxE) w = let (wMinE, wE, wMaxE) = minMaxExcess1 w  in
                                  (minE `min` (wMinE + e), e + wE, maxE `max` (wMaxE + e))
  {-# INLINE minMaxExcess1 #-}

instance MinMaxExcess1 (DVS.Vector Word64) where
  minMaxExcess1 = DVS.foldl gen (0, 0, 0)
    where gen :: (MinExcess, Excess, MaxExcess) -> Word64 -> (MinExcess, Excess, MaxExcess)
          gen (minE, e, maxE) w = let (wMinE, wE, wMaxE) = minMaxExcess1 w  in
                                  (minE `min` (wMinE + e), e + wE, maxE `max` (wMaxE + e))
  {-# INLINE minMaxExcess1 #-}


word8ExcessMin :: DVS.Vector Int
word8ExcessMin =  DVS.fromList
  [ -8, -6, -6, -4, -6, -4, -4, -2, -6, -4, -4, -2, -4, -2, -2,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0, -1,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -5, -3, -3, -1, -3, -1, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -7, -5, -5, -3, -5, -3, -3, -1, -5, -3, -3, -1, -3, -1, -1,  0
  , -5, -3, -3, -1, -3, -1, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -5, -3, -3, -1, -3, -1, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -5, -3, -3, -1, -3, -1, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  , -4, -2, -2,  0, -2,  0, -1,  0, -3, -1, -1,  0, -2,  0, -1,  0
  ]

word8Excess :: DVS.Vector Int
word8Excess =  DVS.fromList
  [ -8, -6, -6, -4, -6, -4, -4, -2, -6, -4, -4, -2, -4, -2, -2,  0
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0,  0,  2
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0,  0,  2
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0,  0,  2
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -2,  0,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  , -6, -4, -4, -2, -4, -2, -2,  0, -4, -2, -2,  0, -2,  0,  0,  2
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -2,  0,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  , -4, -2, -2,  0, -2,  0,  0,  2, -2,  0,  0,  2,  0,  2,  2,  4
  , -2,  0,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  , -2,  0,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  2,  2,  4,  2,  4,  4,  6,  2,  4,  4,  6,  4,  6,  6,  8
  ]

word8ExcessMax :: DVS.Vector Int
word8ExcessMax =  DVS.fromList
  [  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  1,  3,  1,  3,  3,  5
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  1,  3,  1,  3,  3,  5
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  1,  3,  1,  3,  3,  5
  ,  0,  1,  1,  3,  1,  3,  3,  5,  1,  3,  3,  5,  3,  5,  5,  7
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  1,  3,  1,  3,  3,  5
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  1,  0,  2,  0,  1,  1,  3,  0,  1,  0,  2,  0,  2,  2,  4
  ,  0,  1,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  1,  0,  2,  0,  2,  2,  4,  0,  2,  2,  4,  2,  4,  4,  6
  ,  0,  2,  2,  4,  2,  4,  4,  6,  2,  4,  4,  6,  4,  6,  6,  8
  ]
