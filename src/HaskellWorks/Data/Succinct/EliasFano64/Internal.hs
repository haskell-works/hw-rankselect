{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.EliasFano64.Internal
  ( mkHiBits
  , packToWord8
  , packToWord16
  , packToWord32
  , packToWord64
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Widen

mkHiBits :: Count -> [Word64] -> [Bool]
mkHiBits = mkHiBits' 0

mkHiBits' :: Word64 -> Count -> [Word64] -> [Bool]
mkHiBits' _ _ [] = []
mkHiBits' oldHi loBitsSize wws@(w:ws) = case w .>. loBitsSize of
  newHi | oldHi <  newHi  -> False:mkHiBits' (oldHi + 1)  loBitsSize wws
  newHi | oldHi == newHi  -> True :mkHiBits'  oldHi       loBitsSize ws
  _                       -> error "Values must be non-decreasing"

packToWord8 :: [Bool] -> [Word8]
packToWord8 (a:b:c:d:e:f:g:h:xs) =
  (   (if a then 0x01 else 0x00)
  .|. (if b then 0x02 else 0x00)
  .|. (if c then 0x04 else 0x00)
  .|. (if d then 0x08 else 0x00)
  .|. (if e then 0x10 else 0x00)
  .|. (if f then 0x20 else 0x00)
  .|. (if g then 0x40 else 0x00)
  .|. (if h then 0x80 else 0x00)
  ) : packToWord8 xs
packToWord8 [] = []
packToWord8 xs = packToWord8 (take 8 (xs ++ [False]))

packToWord16 :: [Word8] -> [Word16]
packToWord16 (a:b:xs) = (widen16 a .|. (widen16 b .<.  8)):packToWord16 xs
packToWord16 (a  :xs) =  widen16 a                        :packToWord16 xs
packToWord16 []       = []

packToWord32 :: [Word16] -> [Word32]
packToWord32 (a:b:xs) = (widen32 a .|. (widen32 b .<. 16)):packToWord32 xs
packToWord32 (a  :xs) =  widen32 a                        :packToWord32 xs
packToWord32 []       = []

packToWord64 :: [Word32] -> [Word64]
packToWord64 (a:b:xs) = (widen64 a .|. (widen64 b .<. 32)):packToWord64 xs
packToWord64 (a  :xs) =  widen64 a                        :packToWord64 xs
packToWord64 []       = []
