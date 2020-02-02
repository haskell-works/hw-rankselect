{-# OPTIONS_GHC-funbox-strict-fields #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.CsPoppy.Internal.Alpha0
    ( CsPoppyIndex(..)
    , makeCsPoppyIndex
    , makeCsPoppyBlocks
    , makeCsPoppyLayerM2
    , genCsSamples
    , rank0On
    , select0On
    ) where

import Control.DeepSeq
import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Select0
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
makeCsPoppyIndex vv = CsPoppyIndex
  { csPoppyLayerM = layerM
  , csPoppyLayerS = layerS
  }
  where blocks          = makeCsPoppyBlocks vv
        layerM          = makeCsPoppyLayerM2 blocks
        layerMPopCount  = getCsiTotal (CsInterleaved (lastOrZero layerM))
        layerS          = genCsSamples layerMPopCount vv

makeCsPoppyBlocks :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyBlocks vv = DVS.generate (((DVS.length vv + 8 - 1) `div` 8) + 1) genBlocks
  where e = DVS.length vv
        genBlocks :: Int -> Word64
        genBlocks u = let i = fromIntegral u in if
          | (i + 1) * 8 <= e -> let j = i * 8 in
              popCount0 (DVS.unsafeIndex vv (j + 0)) +
              popCount0 (DVS.unsafeIndex vv (j + 1)) +
              popCount0 (DVS.unsafeIndex vv (j + 2)) +
              popCount0 (DVS.unsafeIndex vv (j + 3)) +
              popCount0 (DVS.unsafeIndex vv (j + 4)) +
              popCount0 (DVS.unsafeIndex vv (j + 5)) +
              popCount0 (DVS.unsafeIndex vv (j + 6)) +
              popCount0 (DVS.unsafeIndex vv (j + 7))
          | otherwise -> popCount0 (DVS.take 8 (DVS.drop (i * 8) vv))

makeCsPoppyLayerM2 :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyLayerM2 blocksv = DVS.constructN (((DVS.length blocksv + 4 - 1) `div` 4) + 1) genLayer1
  where genLayer1 :: DVS.Vector Word64 -> Word64
        genLayer1 u = let ui = end u in if ui > 0 && ui * 4 + 4 < end blocksv
          then  let lx = DVS.unsafeIndex u       (fromIntegral (ui     - 1)) in
                let la = DVS.unsafeIndex blocksv (fromIntegral (ui * 4 - 4)) in
                let lb = DVS.unsafeIndex blocksv (fromIntegral (ui * 4 - 3)) in
                let lc = DVS.unsafeIndex blocksv (fromIntegral (ui * 4 - 2)) in
                let ld = DVS.unsafeIndex blocksv (fromIntegral (ui * 4 - 1)) in
                let nx = lx + (la + lb + lc + ld)                            in
                let na = DVS.unsafeIndex blocksv (fromIntegral (ui * 4 + 0)) in
                let nb = DVS.unsafeIndex blocksv (fromIntegral (ui * 4 + 1)) in
                let nc = DVS.unsafeIndex blocksv (fromIntegral (ui * 4 + 2)) in
                (   ( nx         .&. 0x00000000ffffffff)
                .|. ((na .<. 32) .&. 0x000003ff00000000)
                .|. ((nb .<. 42) .&. 0x000ffc0000000000)
                .|. ((nc .<. 52) .&. 0x3ff0000000000000))
          else  let lx = lastOrZero u                      in
                let la = indexOrZero blocksv (ui * 4 - 4)  in
                let lb = indexOrZero blocksv (ui * 4 - 3)  in
                let lc = indexOrZero blocksv (ui * 4 - 2)  in
                let ld = indexOrZero blocksv (ui * 4 - 1)  in
                let nx = lx + (la + lb + lc + ld)         in
                let na = indexOrZero blocksv (ui * 4 + 0)  in
                let nb = indexOrZero blocksv (ui * 4 + 1)  in
                let nc = indexOrZero blocksv (ui * 4 + 2)  in
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
genCsSamplesST pc vv = do
  u :: DVSM.MVector s Word64 <- DVSM.unsafeNew maxSize

  go u 0 0 (DVS.length vv) 0 1
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
          let vw    = comp (DVS.unsafeIndex vv vi)  -- Source word
          let vwpc  = popCount1 vw                  -- Source word pop count
          let npc   = vwpc + lpc                    -- Next pop count

          if npc >= epc
            then do -- Emit a position
              let dpc = epc - lpc
              let ewp = select0 vw dpc + fromIntegral vi * 64 -- Expectant word position
              DVSM.unsafeWrite u ui ewp
              go u (ui + 1) (vi + 1) vie npc (epc + sampleWidth)
            else go u ui (vi + 1) vie npc epc -- Don't emit a position this time
        go u ui _ _ _ _ = return (DVSM.take ui u)

rank0On :: DVS.Vector Word64 -> CsPoppyIndex -> Count -> Count
rank0On v (CsPoppyIndex layerM _) p = rankPrior + rankInBasicBlock
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
        rankInBasicBlock  = rank0 (DVS.drop (fromIntegral (p `div` 512) * 8) v) (p `mod` 512)
{-# INLINE rank0On #-}

select0On :: DVS.Vector Word64 -> CsPoppyIndex -> Count -> Count
select0On _ _ r | r == 0  = 0
select0On v (CsPoppyIndex !layerM !layerS) r =
  let !si                 = (r - 1) `div` 8192                              in
  let !spi                = layerS !!! fromIntegral si                      in
  let vBitSize            = fromIntegral (DVS.length v) * 64                in
  let !spj                = atIndexOr vBitSize layerS (fromIntegral si + 1) in
  let !mi                 = spi `div` (512 * 4)                             in
  let !mj                 = spj `div` (512 * 4)                             in
  let !(!bbr, !bbi)       = lookupLayerMFrom2 mi (mj + 1) r layerM          in
  let !block              = DVS.take 8 (drop (bbi * 8) v)                   in
  let !q                  = r - bbr                                         in

  select0 block q + bbi * 512
{-# INLINE select0On #-}
