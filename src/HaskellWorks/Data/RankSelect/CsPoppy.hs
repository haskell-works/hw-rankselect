{-# OPTIONS_GHC-funbox-strict-fields #-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.CsPoppy
    ( CsPoppy(..)
    , Nice(..)
    , Rank1(..)
    , makeCsPoppy
    , lookupLayerMFrom1
    , lookupLayerMFrom2
    ) where

import Control.DeepSeq
import Data.Monoid                                                 ((<>))
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
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Drop
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select0
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy.Internal.CsInterleaved
import HaskellWorks.Data.RankSelect.CsPoppy.Internal.Lookup
import HaskellWorks.Data.RankSelect.CsPoppy.Internal.Vector
import HaskellWorks.Data.Vector.AsVector64
import Prelude                                                     hiding (drop, length, pi, take)

import qualified Data.Vector.Storable                                 as DVS
import qualified HaskellWorks.Data.RankSelect.CsPoppy.Internal.Alpha0 as A0
import qualified HaskellWorks.Data.RankSelect.CsPoppy.Internal.Alpha1 as A1

newtype Nice a = Nice a deriving Eq

data CsPoppy = CsPoppy
  { csPoppyBits   :: !(DVS.Vector Word64)
  , csPoppyIndex0 :: !A0.CsPoppyIndex
  , csPoppyIndex1 :: !A1.CsPoppyIndex
  } deriving (Eq, Show, NFData, Generic)

instance FromForeignRegion CsPoppy where
  fromForeignRegion = makeCsPoppy . fromForeignRegion

instance Show (Nice CsPoppy) where
  showsPrec _ (Nice rsbs) = showString "CsPoppy "
    <> showString "{ csPoppyBits = "   <> shows (bitShow <$> DVS.toList (csPoppyBits     rsbs))
    <> showString ", csPoppyIndex1 = CsPoppyIndex"
    <> showString "{ csPoppyLayerM = " <> shows (CsInterleaved <$> DVS.toList (A1.csPoppyLayerM (csPoppyIndex1 rsbs)))
    <> showString ", csPoppyLayerS = " <> shows (A1.csPoppyLayerS (csPoppyIndex1 rsbs))
    <> showString " }"
    <> showString " }"

instance AsVector64 CsPoppy where
  asVector64 = asVector64 . csPoppyBits
  {-# INLINE asVector64 #-}

instance BitLength CsPoppy where
  bitLength = bitLength . csPoppyBits
  {-# INLINE bitLength #-}

instance PopCount0 CsPoppy where
  popCount0 v = getCsiTotal (CsInterleaved (lastOrZero (A0.csPoppyLayerM (csPoppyIndex0 v))))
  {-# INLINE popCount0 #-}

instance PopCount1 CsPoppy where
  popCount1 v = getCsiTotal (CsInterleaved (lastOrZero (A1.csPoppyLayerM (csPoppyIndex1 v))))
  {-# INLINE popCount1 #-}

-- TODO Try to get rid of indexOrZero call
makeCsPoppy :: DVS.Vector Word64 -> CsPoppy
makeCsPoppy v = CsPoppy
  { csPoppyBits   = v
  , csPoppyIndex0 = A0.makeCsPoppyIndex v
  , csPoppyIndex1 = A1.makeCsPoppyIndex v
  }

instance TestBit CsPoppy where
  (.?.) = (.?.) . csPoppyBits
  {-# INLINE (.?.) #-}

instance BitRead CsPoppy where
  bitRead = fmap makeCsPoppy . bitRead

instance Rank0 CsPoppy where
  rank0 rsbs p = p - rank1 rsbs p
  {-# INLINE rank0 #-}

instance Rank1 CsPoppy where
  rank1 (CsPoppy !v _ (A1.CsPoppyIndex !layerM !_)) p = rankPrior + rankInBasicBlock
    where mw  = layerM !!! toPosition (p `div` 2048)
          mx  =  mw .&. 0x00000000ffffffff
          ma  = (mw .&. 0x000003ff00000000) .>. 32
          mb  = (mw .&. 0x000ffc0000000000) .>. 42
          mc  = (mw .&. 0x3ff0000000000000) .>. 52
          q   = (p `div` 512) `mod` 4 -- quarter
          mi  | q == 0    = mx
              | q == 1    = mx + ma
              | q == 2    = mx + ma + mb
              | q == 3    = mx + ma + mb + mc
              | otherwise = error "Invalid interleaved entry index"
          rankPrior         = mi :: Count
          rankInBasicBlock  = rank1 (DVS.drop (fromIntegral (p `div` 512) * 8) v) (p `mod` 512)
  {-# INLINE rank1 #-}

instance Select0 CsPoppy where
  select0 _                             r | r == 0  = 0
  select0 (CsPoppy !v (A0.CsPoppyIndex !layerM !layerS) _)  r           =
    let !si                 = (r - 1) `div` 8192                              in
    let !spi                = layerS !!! fromIntegral si                      in
    let vBitSize = fromIntegral (DVS.length v) * 64                           in
    let !spj                = atIndexOr vBitSize layerS (fromIntegral si + 1) in
    let !mi                 = spi `div` (512 * 4)                             in
    let !mj                 = spj `div` (512 * 4)                             in
    -- let !(!bbr, !bbi)     = lookupLayerMFrom1 mi (mj + 1) r layerM            in
    let !(!bbr, !bbi)     = lookupLayerMFrom2 mi (mj + 1) r layerM            in
    let !block              = DVS.take 8 (drop (bbi * 8) v)                   in
    let !q                  = r - bbr                                         in

    select0 block q + bbi * 512
  {-# INLINE select0 #-}

instance Select1 CsPoppy where
  select1 _                             r | r == 0  = 0
  select1 (CsPoppy !v _ (A1.CsPoppyIndex !layerM !layerS))  r           =
    let !si                 = (r - 1) `div` 8192                              in
    let !spi                = layerS !!! fromIntegral si                      in
    let vBitSize = fromIntegral (DVS.length v) * 64                           in
    let !spj                = atIndexOr vBitSize layerS (fromIntegral si + 1) in
    let !mi                 = spi `div` (512 * 4)                             in
    let !mj                 = spj `div` (512 * 4)                             in
    -- let !(!bbr, !bbi)     = lookupLayerMFrom1 mi (mj + 1) r layerM            in
    let !(!bbr, !bbi)     = lookupLayerMFrom2 mi (mj + 1) r layerM            in
    let !block              = DVS.take 8 (drop (bbi * 8) v)                   in
    let !q                  = r - bbr                                         in

    select1 block q + bbi * 512
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

instance BalancedParens CsPoppy where
  firstChild  = firstChild  . csPoppyBits
  nextSibling = nextSibling . csPoppyBits
  parent      = parent      . csPoppyBits
  {-# INLINE firstChild  #-}
  {-# INLINE nextSibling #-}
  {-# INLINE parent      #-}
