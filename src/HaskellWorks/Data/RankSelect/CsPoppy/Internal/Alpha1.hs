{-# OPTIONS_GHC-funbox-strict-fields #-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.CsPoppy.Internal.Alpha1
    ( CsPoppyIndex(..)
    , makeCsPoppyIndex
    , makeCsPoppyBlocks
    , makeCsPoppyLayerM2
    , genCsSamples
    , rank1On
    , select1On
    ) where

import Control.DeepSeq
import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy.Internal.CsInterleaved
import HaskellWorks.Data.RankSelect.CsPoppy.Internal.Lookup
import HaskellWorks.Data.RankSelect.CsPoppy.Internal.Vector

import Prelude hiding (drop)

import qualified Control.Monad.ST             as ST
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

data CsPoppyIndex = CsPoppyIndex
  { csPoppyLayerM :: !(DVS.Vector Word64)
  , csPoppyLayerS :: !(DVS.Vector Word64) -- Sampling lookup of each 8192 1-bit
  } deriving (Eq, Show, NFData, Generic)

makeCsPoppyIndex :: DVS.Vector Word64 -> CsPoppyIndex
makeCsPoppyIndex v = CsPoppyIndex
  { csPoppyLayerM = layerM
  , csPoppyLayerS = layerS
  }
  where blocks          = makeCsPoppyBlocks v
        layerM          = makeCsPoppyLayerM2 blocks
        layerMPopCount  = getCsiTotal (CsInterleaved (lastOrZero layerM))
        layerS          = genCsSamples layerMPopCount v

makeCsPoppyBlocks :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyBlocks v = DVS.generate (((DVS.length v + 8 - 1) `div` 8) + 1) genBlocks
  where e = DVS.length v
        genBlocks :: Int -> Word64
        genBlocks u = let i = fromIntegral u in if
          | (i + 1) * 8 <= e -> let j = i * 8 in
              popCount1 (DVS.unsafeIndex v (j + 0)) +
              popCount1 (DVS.unsafeIndex v (j + 1)) +
              popCount1 (DVS.unsafeIndex v (j + 2)) +
              popCount1 (DVS.unsafeIndex v (j + 3)) +
              popCount1 (DVS.unsafeIndex v (j + 4)) +
              popCount1 (DVS.unsafeIndex v (j + 5)) +
              popCount1 (DVS.unsafeIndex v (j + 6)) +
              popCount1 (DVS.unsafeIndex v (j + 7))
          | otherwise -> popCount1 (DVS.take 8 (DVS.drop (i * 8) v))

makeCsPoppyLayerM2 :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyLayerM2 blocks = DVS.constructN (((DVS.length blocks + 4 - 1) `div` 4) + 1) genLayer1
  where genLayer1 :: DVS.Vector Word64 -> Word64
        genLayer1 u = let ui = end u in if ui > 0 && ui * 4 + 4 < end blocks
          then  let lx = DVS.unsafeIndex u      (fromIntegral (ui     - 1)) in
                let la = DVS.unsafeIndex blocks (fromIntegral (ui * 4 - 4)) in
                let lb = DVS.unsafeIndex blocks (fromIntegral (ui * 4 - 3)) in
                let lc = DVS.unsafeIndex blocks (fromIntegral (ui * 4 - 2)) in
                let ld = DVS.unsafeIndex blocks (fromIntegral (ui * 4 - 1)) in
                let nx = lx + (la + lb + lc + ld)                           in
                let na = DVS.unsafeIndex blocks (fromIntegral (ui * 4 + 0)) in
                let nb = DVS.unsafeIndex blocks (fromIntegral (ui * 4 + 1)) in
                let nc = DVS.unsafeIndex blocks (fromIntegral (ui * 4 + 2)) in
                (   ( nx         .&. 0x00000000ffffffff)
                .|. ((na .<. 32) .&. 0x000003ff00000000)
                .|. ((nb .<. 42) .&. 0x000ffc0000000000)
                .|. ((nc .<. 52) .&. 0x3ff0000000000000))
          else  let lx = lastOrZero u                     in
                let la = indexOrZero blocks (ui * 4 - 4)  in
                let lb = indexOrZero blocks (ui * 4 - 3)  in
                let lc = indexOrZero blocks (ui * 4 - 2)  in
                let ld = indexOrZero blocks (ui * 4 - 1)  in
                let nx = lx + (la + lb + lc + ld)         in
                let na = indexOrZero blocks (ui * 4 + 0)  in
                let nb = indexOrZero blocks (ui * 4 + 1)  in
                let nc = indexOrZero blocks (ui * 4 + 2)  in
                (   ( nx         .&. 0x00000000ffffffff)
                .|. ((na .<. 32) .&. 0x000003ff00000000)
                .|. ((nb .<. 42) .&. 0x000ffc0000000000)
                .|. ((nc .<. 52) .&. 0x3ff0000000000000))

sampleWidth :: Count
sampleWidth = 8192
{-# INLINE sampleWidth #-}

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
          let vw    = DVS.unsafeIndex v vi  -- Source word
          let vwpc  = popCount1 vw          -- Source word pop count
          let npc   = vwpc + lpc            -- Next pop count

          if npc >= epc
            then do -- Emit a position
              let dpc = epc - lpc
              let ewp = select1 vw dpc + fromIntegral vi * 64 -- Expectant word position
              DVSM.unsafeWrite u ui ewp
              go u (ui + 1) (vi + 1) vie npc (epc + sampleWidth)
            else go u ui (vi + 1) vie npc epc -- Don't emit a position this time
        go u ui _ _ _ _ = return (DVSM.take ui u)

rank1On :: DVS.Vector Word64 -> CsPoppyIndex -> Count -> Count
rank1On v (CsPoppyIndex layerM _) p = rankPrior + rankInBasicBlock
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
{-# INLINE rank1On #-}

select1On :: DVS.Vector Word64 -> CsPoppyIndex -> Count -> Count
select1On _ _ r | r == 0  = 0
select1On v (CsPoppyIndex !layerM !layerS) r =
  let !si                 = (r - 1) `div` 8192                              in
  let !spi                = layerS !!! fromIntegral si                      in
  let vBitSize            = fromIntegral (DVS.length v) * 64                in
  let !spj                = atIndexOr vBitSize layerS (fromIntegral si + 1) in
  let !mi                 = spi `div` (512 * 4)                             in
  let !mj                 = spj `div` (512 * 4)                             in
  let !(!bbr, !bbi)       = lookupLayerMFrom2 mi (mj + 1) r layerM          in
  let !block              = DVS.take 8 (drop (bbi * 8) v)                   in
  let !q                  = r - bbr                                         in

  select1 block q + bbi * 512
{-# INLINE select1On #-}
