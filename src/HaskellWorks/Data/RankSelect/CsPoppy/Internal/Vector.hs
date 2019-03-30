module HaskellWorks.Data.RankSelect.CsPoppy.Internal.Vector
  ( indexOrZero
  , lastOrZero
  ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Positioning

import qualified Data.Vector.Storable as DVS

indexOrZero :: DVS.Vector Word64 -> Position -> Word64
indexOrZero _ i | i < 0     = 0
indexOrZero v i | i < end v = v !!! i
indexOrZero _ _ = 0
{-# INLINE indexOrZero #-}

lastOrZero :: DVS.Vector Word64 -> Word64
lastOrZero v | 0 < end v  = DVS.last v
lastOrZero _ = 0
{-# INLINE lastOrZero #-}
