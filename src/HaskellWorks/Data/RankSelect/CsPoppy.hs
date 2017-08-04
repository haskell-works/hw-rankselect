{-# OPTIONS_GHC-funbox-strict-fields #-}

-- {-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}

module HaskellWorks.Data.RankSelect.CsPoppy
    ( CsPoppy(..)
    , Nice(..)
    , Rank1(..)
    , makeCsPoppy
    -- , sampleRange
    , genSample
    , chunkBy
    , track
    ) where

import GHC.Generics
import Control.DeepSeq
import Data.Word
import Data.Monoid ((<>))
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsInterleaved
import HaskellWorks.Data.Take
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Search
import HaskellWorks.Data.Vector.AsVector64
import Prelude hiding (length, take, drop)

import Debug.Trace

import qualified Data.Vector.Storable as DVS

newtype Nice a = Nice a deriving Eq

data CsPoppy = CsPoppy
  { csPoppyBits   :: !(DVS.Vector Word64)
  , csPoppyBlocks :: !(DVS.Vector Word64)
  , csPoppyLayer1 :: !(DVS.Vector Word64)
  -- , csPoppyLayer0 :: !(DVS.Vector Word64)
  , csPoppyLayerS   :: !(DVS.Vector Word64) -- Sampling position of each 8192 1-bit
  } deriving (Eq, Show, NFData, Generic)


chunkBy :: Int -> DVS.Vector Word64 -> [DVS.Vector Word64]
chunkBy n v | DVS.length v > 0  = DVS.take n v:chunkBy n (DVS.drop n v)
chunkBy _ _                     = []

-- chunkBy :: Int -> DVS.Vector Word64 -> [DVS.Vector Word64]
-- chunkBy n v | DVS.length v > 0  = DVS.take n v:chunkBy n (DVS.drop n v)
-- chunkBy _ _                     = []

instance Show (Nice CsPoppy) where
  showsPrec _ (Nice rsbs) = showString "CsPoppy "
    <> showString "{ csPoppyBits = "   <> shows (bitShow <$> DVS.toList (csPoppyBits     rsbs))
    <> showString ", csPoppyBlocks = " <> shows (chunkBy 4 (csPoppyBlocks rsbs))
    <> showString ", csPoppyLayer0 = " <> shows (CsInterleaved <$> DVS.toList (csPoppyLayer1 rsbs))
    <> showString ", csPoppyLayerS = " <> shows (csPoppyLayerS rsbs)
    <> showString " }"

instance AsVector64 CsPoppy where
  asVector64 = asVector64 . csPoppyBits
  {-# INLINE asVector64 #-}

instance BitLength CsPoppy where
  bitLength = bitLength . csPoppyBits
  {-# INLINE bitLength #-}

instance PopCount1 CsPoppy where
  popCount1 = popCount1 . csPoppyBits
  {-# INLINE popCount1 #-}
--
-- popCount1Range :: (DVS.Storable a, PopCount1 a) => Int -> Int -> DVS.Vector a -> Count
-- popCount1Range start len = popCount1 . DVS.take len . DVS.drop start
--
-- indexAsPos :: DVS.Vector Word64 -> Position -> Word64
-- indexAsPos _ i | i < 0     = 0
-- indexAsPos v i | i < end v = v !!! i
-- indexAsPos v _ | 0 < end v = DVS.last v
-- indexAsPos v _             = DVS.last v
-- {-# INLINE indexAsPos #-}

indexOrZero :: DVS.Vector Word64 -> Position -> Word64
indexOrZero _ i | i < 0     = 0
indexOrZero v i | i < end v = v !!! i
indexOrZero _ _             = 0
{-# INLINE indexOrZero #-}

lastOrZero :: DVS.Vector Word64 -> Word64
lastOrZero v | 0 < end v  = DVS.last v
lastOrZero _              = 0

makeCsPoppy :: DVS.Vector Word64 -> CsPoppy
makeCsPoppy v = CsPoppy
  { csPoppyBits   = v
  , csPoppyBlocks = blocks
  , csPoppyLayer1 = layer1
  -- , csPoppyLayer0 = DVS.constructN (((DVS.length v + 0x100000000 - 1) `div` 0x100000000) + 1) genLayer0
  , csPoppyLayerS = layerS
  }
  where blocks = DVS.constructN (((DVS.length v       + 8 - 1) `div` 8) + 1) genBlocks
        layer1 = DVS.constructN (((DVS.length blocks  + 4 - 1) `div` 4) + 1) genLayer1
        layerS = DVS.unfoldrN (fromIntegral (popCount1 v `div` selectBlockSize) + 1) (genSample v (popCount1 v)) 1
        genBlocks u = let i = length u in popCount1 (take 8 (drop (i * 8) v))
        genLayer1 :: DVS.Vector Word64 -> Word64
        genLayer1 u =
          let i  = end u                          in
          let lx = lastOrZero u                   in
          let la = indexOrZero blocks (i * 4 - 4) in
          let lb = indexOrZero blocks (i * 4 - 3) in
          let lc = indexOrZero blocks (i * 4 - 2) in
          let ld = indexOrZero blocks (i * 4 - 1) in
          let nx = lx + (la + lb + lc + ld)       in
          let na = indexOrZero blocks (i * 4 + 0) in
          let nb = indexOrZero blocks (i * 4 + 1) in
          let nc = indexOrZero blocks (i * 4 + 2) in
          (   ( nx         .&. 0x00000000ffffffff)
          .|. ((na .<. 32) .&. 0x000003ff00000000)
          .|. ((nb .<. 42) .&. 0x000ffc0000000000)
          .|. ((nc .<. 52) .&. 0x3ff0000000000000))

selectBlockSize :: Count
selectBlockSize = 8192

track :: Show a => String -> a -> a
track name a = trace (name <> ": " <> show a) a

-- mpc = max pop count
genSample :: DVS.Vector Word64 -> Count -> Count -> Maybe (Word64, Count)
genSample v mpc r | r <= mpc      = Just (fromIntegral (select1 v r), r + selectBlockSize)
genSample v _   r | r < maxBound  = Just (fromIntegral (DVS.length v * 64), maxBound)
genSample _ _   _                 = Nothing

instance TestBit CsPoppy where
  (.?.) = (.?.) . csPoppyBits
  {-# INLINE (.?.) #-}

instance BitRead CsPoppy where
  bitRead = fmap makeCsPoppy . bitRead

instance Rank1 CsPoppy where
  rank1 (CsPoppy v _ layer1 _) p = rankPrior + rankInBasicBlock
    where -- rankLayer0              = layer0  !!! toPosition (p `div` 0x100000000)
          rankLayer1Word          = layer1 !!! toPosition (p `div` 2048)
          rankLayer1X             =  rankLayer1Word .&. 0x00000000ffffffff
          rankLayer1A             = (rankLayer1Word .&. 0x000003ff00000000) .>. 32
          rankLayer1B             = (rankLayer1Word .&. 0x000ffc0000000000) .>. 42
          rankLayer1C             = (rankLayer1Word .&. 0x3ff0000000000000) .>. 52
          q                       = (p `div` 512) `mod` 4 -- quarter
          rankLayer1  | q == 0    = rankLayer1X
                      | q == 1    = rankLayer1X + rankLayer1A
                      | q == 2    = rankLayer1X + rankLayer1A + rankLayer1B
                      | q == 3    = rankLayer1X + rankLayer1A + rankLayer1B + rankLayer1C
                      | otherwise = undefined
          rankPrior               = {-rankLayer0 +-} rankLayer1 :: Count
          rankInBasicBlock        = rank1 (DVS.drop (fromIntegral (p `div` 512) * 8) v) (p `mod` 512)

instance Select1 CsPoppy where
  select1 (CsPoppy v _ layer1 _) p =
    -- let !_ = track "selecting" p in
    -- Function that looks up a Layer 1X index by index
    let layer1Lookup i = get1a (CsInterleaved (layer1 !!! i)) in
    -- Index chosen by binary search
    let layer1Index = binarySearch (fromIntegral p) layer1Lookup 0 (fromIntegral $ DVS.length layer1 - 1) in
    -- let !_ = track "cspoppy" rsbs                  in
    -- let !_ = track "layer1Index" layer1Index                  in
    let layer1Entry = CsInterleaved (layer1 !!! layer1Index)  in
    -- let !_ = track "layer1Entry" layer1Entry                  in
    let entryX = get1a layer1Entry                            in
    let entryA = get2a layer1Entry + entryX                   in
    let entryB = get2b layer1Entry + entryA                   in
    let entryC = get2c layer1Entry + entryB                   in
    -- let !_ = track "entryX" entryX                            in
    -- let !_ = track "entryA" entryA                            in
    -- let !_ = track "entryB" entryB                            in
    -- let !_ = track "entryC" entryC                            in
    let blockOffset | p <= entryA = 0
                    | p <= entryB = 1
                    | p <= entryC = 2
                    | otherwise  = 3                          in
    -- let !_ = track "blockOffset" blockOffset                  in
    let blockPrepop | p <= entryA = entryX
                    | p <= entryB = entryA
                    | p <= entryC = entryB
                    | otherwise  = entryC                         in
    let blockStart = toCount (layer1Index * 4 + blockOffset) * 8  in
    -- let !_ = track "blockStart" blockStart                        in
    let block = DVS.take 8 (drop blockStart v)                    in
    -- let !_ = track "blockSize" (length block)                     in
    -- let !_ = track "block" block                                  in
    -- let !_ = track "blockPrepop" blockPrepop                      in
    let q = p - blockPrepop                                       in
    -- let !_ = track "q" q                                          in
    select1 block q + blockStart * 64

  -- select1 block (p - )
  --   let _ = p `div` selectBlockSize in
  --   let !_ = track "sampleIndex" sampleIndex in
  --   let !_ = track "selectMin" selectMin in
  --   let !_ = track "selectMax" selectMax in
  --   if DVS.length v /= 0
  --       then toCount q * 512 + select1 (DVS.drop (fromIntegral q * 8) v) (p - s)
  --       else 0
  --   where sampleIndex = binarySearch (fromIntegral p) (layerS !!!) 0 (end layerS - 1)
  --         selectMin = layerS !!! sampleIndex
  --         selectMax = layerS !!! (sampleIndex + 1)
  --         q = binarySearch (fromIntegral p) wordAt (iMin) iMax
  --         s = (i !!! q) :: Count
  --         wordAt = (i !!!)
  --         iMin = fromIntegral $  (sampleMin - 1) `div` 512      :: Position
  --         iMax = fromIntegral $ ((sampleMax - 1) `div` 512) + 1 :: Position

-- sampleRange :: CsPoppy -> Count -> (Word64, Word64)
-- sampleRange (CsPoppy _ index _ samples) p =
--   let j = (fromIntegral p - 1) `div` 8192         in
--   if 0 <= j && j < DVS.length samples
--     then  let pa = samples DVS.! j                in
--           if j + 1 < DVS.length samples
--             then  let pz = samples DVS.! (j + 1)  in
--                   (pa, pz)
--             else (pa, fromIntegral (DVS.length index - 1))
--     else (1, fromIntegral (DVS.length index - 1))
