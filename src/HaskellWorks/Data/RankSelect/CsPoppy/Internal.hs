{-# OPTIONS_GHC-funbox-strict-fields #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.CsPoppy.Internal
    ( CsInterleaved(..)
    , getCsiX
    , getCsiA
    , getCsiB
    , getCsiC
    , getCsiTotal
    , mkCsi
    , putCsiX
    , putCsiA
    , putCsiB
    , putCsiC
    , indexOrZero
    , lastOrZero
    , makeCsPoppyBlocks1
    , makeCsPoppyBlocks2
    , makeCsPoppyLayerM
    , genCsSamples
    ) where

import Data.Word
import Foreign.Ptr
import Foreign.Storable
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Select1

import qualified Control.Monad.ST             as ST
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

newtype CsInterleaved = CsInterleaved { unCsInterleaved :: Word64 } deriving Eq

instance Storable CsInterleaved where
  sizeOf = sizeOf . unCsInterleaved
  {-# INLINE sizeOf #-}
  alignment = alignment . unCsInterleaved
  {-# INLINE alignment #-}
  peekElemOff ptr i = CsInterleaved <$> peekElemOff (castPtr ptr) i
  {-# INLINE peekElemOff #-}
  pokeElemOff ptr i = pokeElemOff (castPtr ptr) i . unCsInterleaved
  {-# INLINE pokeElemOff #-}

mkCsi :: Word64 -> Word64 -> Word64 -> Word64 -> CsInterleaved
mkCsi x a b c = CsInterleaved
    $   ((x .&. 0xffffffff) .<.  0)
    .|. ((a .&.      0x3ff) .<. 32)
    .|. ((b .&.      0x3ff) .<. 42)
    .|. ((c .&.      0x3ff) .<. 52)

getCsiX :: CsInterleaved -> Word64
getCsiX (CsInterleaved i) = i .&. 0xffffffff

getCsiA :: CsInterleaved -> Word64
getCsiA (CsInterleaved i) = (i .>. 32) .&. 0x3ff

getCsiB :: CsInterleaved -> Word64
getCsiB (CsInterleaved i) = (i .>. 42) .&. 0x3ff

getCsiC :: CsInterleaved -> Word64
getCsiC (CsInterleaved i) = (i .>. 52) .&. 0x3ff

getCsiTotal :: CsInterleaved -> Word64
getCsiTotal csi = getCsiX csi + getCsiA csi + getCsiB csi + getCsiC csi

putCsiX :: Word64 -> CsInterleaved -> CsInterleaved
putCsiX v (CsInterleaved i) = CsInterleaved (((v .&. 0xffffffff) .<.  0) .|. (i .&. 0xffffffff00000000))

putCsiA :: Word64 -> CsInterleaved -> CsInterleaved
putCsiA v (CsInterleaved i) = CsInterleaved (((v .&.      0x3ff) .<. 32) .|. (i .&. 0xfffffc00ffffffff))

putCsiB :: Word64 -> CsInterleaved -> CsInterleaved
putCsiB v (CsInterleaved i) = CsInterleaved (((v .&.      0x3ff) .<. 42) .|. (i .&. 0xfff003ffffffffff))

putCsiC :: Word64 -> CsInterleaved -> CsInterleaved
putCsiC v (CsInterleaved i) = CsInterleaved (((v .&.      0x3ff) .<. 52) .|. (i .&. 0xc00fffffffffffff))

instance Show CsInterleaved where
  showsPrec _ i = shows (getCsiX i, getCsiA i, getCsiB i, getCsiC i)

makeCsPoppyBlocks1 :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyBlocks1 v = DVS.constructN (((DVS.length v + 8 - 1) `div` 8) + 1) genBlocks
  where genBlocks :: DVS.Vector Word64 -> Word64
        genBlocks u = let i = DVS.length u in popCount1 (DVS.take 8 (DVS.drop (i * 8) v))

makeCsPoppyBlocks2 :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyBlocks2 v = DVS.constructN (((DVS.length v + 8 - 1) `div` 8) + 1) genBlocks
  where e = DVS.length v
        genBlocks :: DVS.Vector Word64 -> Word64
        genBlocks u = let i = DVS.length u in if
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

makeCsPoppyLayerM :: DVS.Vector Word64 -> DVS.Vector Word64
makeCsPoppyLayerM blocks = DVS.constructN (((DVS.length blocks + 4 - 1) `div` 4) + 1) genLayer1
  where genLayer1 :: DVS.Vector Word64 -> Word64
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

indexOrZero :: DVS.Vector Word64 -> Position -> Word64
indexOrZero _ i | i < 0     = 0
indexOrZero v i | i < end v = v !!! i
indexOrZero _ _ = 0
{-# INLINE indexOrZero #-}

lastOrZero :: DVS.Vector Word64 -> Word64
lastOrZero v | 0 < end v  = DVS.last v
lastOrZero _ = 0
{-# INLINE lastOrZero #-}

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
          let vw = DVS.unsafeIndex v vi -- Source word
          let vwpc = popCount1 vw       -- Source word pop count
          let npc = vwpc + lpc          -- Next pop count

          if npc >= epc
            then do -- Emit a position
              let dpc = epc - lpc
              let ewp = select1 vw dpc + fromIntegral vi * 64 -- Expectant word position
              DVSM.unsafeWrite u ui ewp
              go u (ui + 1) (vi + 1) vie npc (epc + sampleWidth)
            else do -- Don't emit a position this time
              go u ui (vi + 1) vie npc epc
        go u ui _ _ _ _ = return (DVSM.take ui u)
