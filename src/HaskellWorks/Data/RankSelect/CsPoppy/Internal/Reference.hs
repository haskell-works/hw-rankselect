{-# OPTIONS_GHC-funbox-strict-fields #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.RankSelect.CsPoppy.Internal.Reference
    ( makeCsPoppyLayerM
    ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Length
import HaskellWorks.Data.RankSelect.CsPoppy.Internal.Vector

import qualified Data.Vector.Storable as DVS

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

