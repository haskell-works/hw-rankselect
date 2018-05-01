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
    , genCsSamples
    , layerMPositions
    , lookupLayerMFrom1
    , lookupLayerMFrom2
    ) where

import Control.DeepSeq
import Data.Int
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
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy.Internal
import HaskellWorks.Data.Take
import HaskellWorks.Data.Vector.AsVector64
import Prelude                                         hiding (drop, length, pi, take)

import qualified Control.Monad.ST             as ST
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

newtype Nice a = Nice a deriving Eq

data CsPoppy = CsPoppy
  { csPoppyBits   :: !(DVS.Vector Word64)
  , csPoppyLayerM :: !(DVS.Vector Word64)
  , csPoppyLayerS :: !(DVS.Vector Word64) -- Sampling lookup of each 8192 1-bit
  } deriving (Eq, Show, NFData, Generic)

instance FromForeignRegion CsPoppy where
  fromForeignRegion = makeCsPoppy . fromForeignRegion

instance Show (Nice CsPoppy) where
  showsPrec _ (Nice rsbs) = showString "CsPoppy "
    <> showString "{ csPoppyBits = "   <> shows (bitShow <$> DVS.toList (csPoppyBits     rsbs))
    <> showString ", csPoppyLayer0 = " <> shows (CsInterleaved <$> DVS.toList (csPoppyLayerM rsbs))
    <> showString ", csPoppyLayerS = " <> shows (csPoppyLayerS rsbs)
    <> showString " }"

instance AsVector64 CsPoppy where
  asVector64 = asVector64 . csPoppyBits
  {-# INLINE asVector64 #-}

instance BitLength CsPoppy where
  bitLength = bitLength . csPoppyBits
  {-# INLINE bitLength #-}

instance PopCount1 CsPoppy where
  popCount1 v = getCsiTotal (CsInterleaved (lastOrZero (csPoppyLayerM v)))
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
  , csPoppyLayerM = layerM
  , csPoppyLayerS = layerS
  }
  where blocks  = DVS.constructN (((DVS.length v      + 8 - 1) `div` 8) + 1) genBlocks
        layerM  = DVS.constructN (((DVS.length blocks + 4 - 1) `div` 4) + 1) genLayer1
        layerMPopCount = getCsiTotal (CsInterleaved (lastOrZero layerM))
        layerS = genCsSamples layerMPopCount v
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

sampleWidth :: Count
sampleWidth = 8192

genCsSamples :: Count -> DVS.Vector Word64 -> DVS.Vector Word64
genCsSamples pc v = DVS.create (genCsSamplesST pc v)

genCsSamplesST :: Count -> DVS.Vector Word64 -> (forall s . ST.ST s (DVS.MVector s Word64))
genCsSamplesST pc v = do
  u :: DVSM.MVector s Word64 <- DVSM.unsafeNew maxSize

  go u 0 0 (DVS.length v) 0 1
  where maxSize :: Int
        maxSize = fromIntegral $ (pc `div` 8192) + 1
        go :: DVS.MVector s Word64 -> Int -> Int -> Int -> Count -> Count -> ST.ST s (DVS.MVector s Word64)
        go u ui vi vie lpc epc | vi < vie = do
          -- u:   Target vector
          -- ui:  Target vector index
          -- uim: Target vector end index
          -- v:   Source vector
          -- vi:  Target vector index
          -- lpc: Last pop count
          -- epc: Expectant pop count

          -- uw <- DVSM.read u ui
          let vw = v DVS.! vi -- Source word
          let vwpc = popCount1 vw       -- Source word pop count
          let npc = vwpc + lpc          -- Next pop count

          if npc >= epc
            then do -- Emit a position
              let dpc = epc - lpc
              let ewp = select1 vw dpc + fromIntegral vi * 64 -- Expectant word position
              DVSM.write u ui ewp
              go u (ui + 1) (vi + 1) vie npc (epc + sampleWidth)
            else do -- Don't emit a position this time
              go u ui (vi + 1) vie npc epc
        go u ui _ _ _ _ = return (DVSM.take ui u)

instance TestBit CsPoppy where
  (.?.) = (.?.) . csPoppyBits
  {-# INLINE (.?.) #-}

instance BitRead CsPoppy where
  bitRead = fmap makeCsPoppy . bitRead

instance Rank1 CsPoppy where
  rank1 (CsPoppy !v !layerM !_) p = rankPrior + rankInBasicBlock
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

binarySearchPBounds :: (Word64 -> Bool) -> (DVS.Vector Word64) -> Word64 -> Word64 -> Word64
binarySearchPBounds p v = loop
  where loop :: Word64 -> Word64 -> Word64
        loop !l !u
          | u <= l    = l
          | otherwise = let e = v !!! fromIntegral k in if p e then loop l k else loop (k + 1) u
          where k = (u + l) .>. 1
{-# INLINE binarySearchPBounds #-}

layerMPositions :: CsPoppy -> [Word64]
layerMPositions (CsPoppy _ v _) = DVS.toList v >>= expand
  where expand mw = [nx, na, nb, nc]
          where mx  =  mw .&. 0x00000000ffffffff          :: Word64
                ma  = (mw .&. 0x000003ff00000000) .>. 32  :: Word64
                mb  = (mw .&. 0x000ffc0000000000) .>. 42  :: Word64
                mc  = (mw .&. 0x3ff0000000000000) .>. 52  :: Word64
                nx  = mx                                  :: Word64
                na  = nx + ma                             :: Word64
                nb  = na + mb                             :: Word64
                nc  = nb + mc                             :: Word64

lookupLayerMXFrom :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> Word64
lookupLayerMXFrom prev i r v | i < length v =
  let mw  = v !!! fromIntegral i                      :: Word64
      mx  =      ( mw .&. 0x00000000ffffffff        ) :: Word64
  in if r <= mx
    then prev
    else lookupLayerMXFrom i (i + 1) r v
lookupLayerMXFrom prev _ _ _ = prev
{-# INLINE lookupLayerMXFrom #-}

lookupLayerMFrom1 :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> (Word64, Word64)
lookupLayerMFrom1 i _ r v
  | r <= mx   = error "This shouldn't happen"
  | r <= ma   = (mx, j * 4    )
  | r <= mb   = (ma, j * 4 + 1)
  | r <= mc   = (mb, j * 4 + 2)
  | otherwise = (mc, j * 4 + 3)
  where j   = lookupLayerMXFrom 0 i r v
        mw  = v !!! fromIntegral j                      :: Word64
        mx  =      ( mw .&. 0x00000000ffffffff        ) :: Word64
        ma  = mx + ((mw .&. 0x000003ff00000000) .>. 32) :: Word64
        mb  = ma + ((mw .&. 0x000ffc0000000000) .>. 42) :: Word64
        mc  = mb + ((mw .&. 0x3ff0000000000000) .>. 52) :: Word64
{-# INLINE lookupLayerMFrom1 #-}

lookupLayerMFrom2 :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> (Word64, Word64)
lookupLayerMFrom2 i j r v =
  let cmp w = (r - 1) < (w .&. 0xffffffff)
      !k  = (binarySearchPBounds cmp v i j - 1) `max` 0
      !mw = v !!! fromIntegral k                      :: Word64
      !mx =      ( mw .&. 0x00000000ffffffff        ) :: Word64
      !ma = mx + ((mw .&. 0x000003ff00000000) .>. 32) :: Word64
      !mb = ma + ((mw .&. 0x000ffc0000000000) .>. 42) :: Word64
      !mc = mb + ((mw .&. 0x3ff0000000000000) .>. 52) :: Word64
  in if
    | r <= mx   -> (0 , 0        )
    | r <= ma   -> (mx, k * 4    )
    | r <= mb   -> (ma, k * 4 + 1)
    | r <= mc   -> (mb, k * 4 + 2)
    | otherwise -> (mc, k * 4 + 3)
{-# INLINE lookupLayerMFrom2 #-}

------------------------------------------------------------------------------------------------------------------------
-- TODO Write a function to extract parts of LayerM
------------------------------------------------------------------------------------------------------------------------

-- atIndexOrLast :: DVS.Vector Word64 -> Word64 -> Word64
-- atIndexOrLast v i
--   | i < fromIntegral (DVS.length v) = v !!! fromIntegral i
--   | otherwise                       = v !!! fromIntegral (DVS.length v - 1)

instance Select1 CsPoppy where
  select1 _                             r | r == 0  = 0
  select1 (CsPoppy !v !layerM !layerS)  r           =
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
