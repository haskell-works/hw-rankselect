{-# OPTIONS_GHC-funbox-strict-fields #-}

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.RankSelect.CsPoppy
    ( CsPoppy(..)
    , Nice(..)
    , Rank1(..)
    , makeCsPoppy
    ) where

import Control.DeepSeq
import Data.Monoid                                     ((<>))
import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.CloseAt
import HaskellWorks.Data.BalancedParens.Enclose
import HaskellWorks.Data.BalancedParens.FindClose
import HaskellWorks.Data.BalancedParens.FindCloseN
import HaskellWorks.Data.BalancedParens.FindOpen
import HaskellWorks.Data.BalancedParens.FindOpenN
import HaskellWorks.Data.BalancedParens.NewCloseAt
import HaskellWorks.Data.BalancedParens.OpenAt
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsInterleaved
import HaskellWorks.Data.Search
import HaskellWorks.Data.Take
import HaskellWorks.Data.Vector.AsVector64
import Prelude                                         hiding (drop, length, pi, take)

import qualified Data.Vector.Storable as DVS

newtype Nice a = Nice a deriving Eq

data CsPoppy = CsPoppy
  { csPoppyBits   :: !(DVS.Vector Word64)
  , csPoppyLayer1 :: !(DVS.Vector Word64)
  , csPoppyLayerS :: !(DVS.Vector Word32) -- Sampling lookup of each 8192 1-bit
  } deriving (Eq, Show, NFData, Generic)

instance Show (Nice CsPoppy) where
  showsPrec _ (Nice rsbs) = showString "CsPoppy "
    <> showString "{ csPoppyBits = "   <> shows (bitShow <$> DVS.toList (csPoppyBits     rsbs))
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
  popCount1 v = getCsiTotal (CsInterleaved (lastOrZero (csPoppyLayer1 v)))
  {-# INLINE popCount1 #-}

indexOrZero :: DVS.Vector Word64 -> Position -> Word64
indexOrZero _ i | i < 0     = 0
indexOrZero v i | i < end v = v !!! i
indexOrZero _ _ = 0
{-# INLINE indexOrZero #-}

lastOrZero :: DVS.Vector Word64 -> Word64
lastOrZero v | 0 < end v  = DVS.last v
lastOrZero _ = 0
{-# INLINE lastOrZero #-}

makeCsPoppy :: DVS.Vector Word64 -> CsPoppy
makeCsPoppy v = CsPoppy
  { csPoppyBits   = v
  , csPoppyLayer1 = layer1
  , csPoppyLayerS = layerS
  }
  where blocks  = DVS.constructN (((DVS.length v      + 8 - 1) `div` 8) + 1) genBlocks
        layer1  = DVS.constructN (((DVS.length blocks + 4 - 1) `div` 4) + 1) genLayer1
        layer1PopCount = getCsiTotal (CsInterleaved (lastOrZero layer1))
        layerS = DVS.unfoldrN (fromIntegral layer1PopCount) genSample (0, 1)
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
        mLookup :: Position -> Word64
        mLookup i = getCsiX (CsInterleaved (layer1 !!! i))
        genSample :: (Position, Position) -> Maybe (Word32, (Position, Position))
        genSample (mi, _) | mi == maxBound                                            = Nothing
        genSample (mi, _) | mi >= end layer1                                          = Just (fromIntegral (DVS.length layer1 - 1), (maxBound, maxBound))
        genSample (mi, s) | s < toPosition (getCsiTotal (CsInterleaved (mLookup mi))) = Just (fromIntegral mi - 1, (mi, s + 8192))
        genSample (mi, s) = genSample (fromIntegral mi + 1, s)

instance TestBit CsPoppy where
  (.?.) = (.?.) . csPoppyBits
  {-# INLINE (.?.) #-}

instance BitRead CsPoppy where
  bitRead = fmap makeCsPoppy . bitRead

instance Rank1 CsPoppy where
  rank1 (CsPoppy v layer1 _) p = rankPrior + rankInBasicBlock
    where rankLayer1Word          = layer1 !!! toPosition (p `div` 2048)
          rankLayer1X             =  rankLayer1Word .&. 0x00000000ffffffff
          rankLayer1A             = (rankLayer1Word .&. 0x000003ff00000000) .>. 32
          rankLayer1B             = (rankLayer1Word .&. 0x000ffc0000000000) .>. 42
          rankLayer1C             = (rankLayer1Word .&. 0x3ff0000000000000) .>. 52
          q                       = (p `div` 512) `mod` 4 -- quarter
          rankLayer1  | q == 0    = rankLayer1X
                      | q == 1    = rankLayer1X + rankLayer1A
                      | q == 2    = rankLayer1X + rankLayer1A + rankLayer1B
                      | q == 3    = rankLayer1X + rankLayer1A + rankLayer1B + rankLayer1C
                      | otherwise = error "Invalid interleaved entry index"
          rankPrior               = rankLayer1 :: Count
          rankInBasicBlock        = rank1 (DVS.drop (fromIntegral (p `div` 512) * 8) v) (p `mod` 512)
  {-# INLINE rank1 #-}

instance Select1 CsPoppy where
  select1 _                         p | p == 0  = 0
  select1 (CsPoppy v layer1 layerS) p           =
    let pi                  = toPosition $ (p - 1) `div` 8192                 in
    let pj                  = (pi + 1) `min` (end layerS - 1)                 in
    let si                  = fromIntegral   (layerS !!! pi)                  in
    let sj                  = fromIntegral $ (layerS !!! pj) + 1              in
    let mLookup i           = getCsiX (CsInterleaved (layer1 !!! i))          in
    let mIndex              = binarySearch (fromIntegral p) mLookup si sj     in
    let me                  = CsInterleaved (layer1 !!! fromIntegral mIndex)  in
    let mx                  = getCsiX me                                      in
    let ma                  = getCsiA me + mx                                 in
    let mb                  = getCsiB me + ma                                 in
    let mc                  = getCsiC me + mb                                 in
    let bo  | p <= ma       = 0
            | p <= mb       = 1
            | p <= mc       = 2
            | otherwise     = 3                                               in
    let bp  | p <= ma       = mx
            | p <= mb       = ma
            | p <= mc       = mb
            | otherwise     = mc                                              in
    let blockStart          = toCount (mIndex * 4 + bo) * 8                   in
    let block               = DVS.take 8 (drop blockStart v)                  in
    let q                   = p - bp                                          in
    select1 block q + blockStart * 64
  {-# INLINE select1 #-}

instance OpenAt CsPoppy where
  openAt = openAt . csPoppyBits
  {-# INLINE openAt #-}

instance CloseAt CsPoppy where
  closeAt = closeAt . csPoppyBits
  {-# INLINE closeAt #-}

instance NewCloseAt CsPoppy where
  newCloseAt = newCloseAt . csPoppyBits
  {-# INLINE newCloseAt #-}

instance FindOpenN CsPoppy where
  findOpenN = findOpenN . csPoppyBits
  {-# INLINE findOpenN #-}

instance FindOpen CsPoppy where
  findOpen = findOpen . csPoppyBits
  {-# INLINE findOpen #-}

instance FindClose CsPoppy where
  findClose = findClose . csPoppyBits
  {-# INLINE findClose #-}

instance FindCloseN CsPoppy where
  findCloseN = findCloseN . csPoppyBits
  {-# INLINE findCloseN #-}

instance Enclose CsPoppy where
  enclose = enclose . csPoppyBits
  {-# INLINE enclose #-}

instance Rank0 CsPoppy where
  rank0 rsbs p = p - rank0 rsbs p
  {-# INLINE rank0 #-}

instance BalancedParens CsPoppy where
  firstChild  = firstChild  . csPoppyBits
  nextSibling = nextSibling . csPoppyBits
  parent      = parent      . csPoppyBits
  {-# INLINE firstChild  #-}
  {-# INLINE nextSibling #-}
  {-# INLINE parent      #-}
