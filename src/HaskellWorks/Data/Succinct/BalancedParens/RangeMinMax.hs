{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}

module HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax
  ( RangeMinMaxL0(..)
  , mkRangeMinMaxL0
  ) where

import           Data.Int
import qualified Data.Vector                                        as DV
import qualified Data.Vector.Storable                               as DVS
import           Data.Word
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.Excess.MinMaxExcess1
import           HaskellWorks.Data.Vector.VectorLike

data RangeMinMaxL0 = RangeMinMaxL0
  { rangeMinMaxBP       :: DVS.Vector Word64
  , rangeMinMaxL0Min    :: DVS.Vector Int8
  , rangeMinMaxL0Max    :: DVS.Vector Int8
  }

mkRangeMinMaxL0 :: DVS.Vector Word64 -> RangeMinMaxL0
mkRangeMinMaxL0 bp = RangeMinMaxL0
  { rangeMinMaxBP     = bp
  , rangeMinMaxL0Min  = DVS.constructN len0 (\v -> let (minE, _, _) = allMinMax DV.! DVS.length v in fromIntegral minE)
  , rangeMinMaxL0Max  = DVS.constructN len0 (\v -> let (_, _, maxE) = allMinMax DV.! DVS.length v in fromIntegral maxE)
  }
  where len0        = (DVS.length bp + 7) `div` 8
        allMinMax   = DV.constructN len0 (\v -> minMaxExcess1 (bp !!! fromIntegral (DV.length v)))

instance BalancedParens RangeMinMaxL0 where
    findOpen :: RangeMinMaxL0 -> Count -> Maybe Count
    findOpen (RangeMinMaxL0 bp mins maxs) c = undefined
    findClose :: RangeMinMaxL0 -> Count -> Maybe Count
    enclose :: RangeMinMaxL0 -> Count -> Maybe Count
    firstChild :: RangeMinMaxL0 -> Count -> Maybe Count
    nextSibling :: RangeMinMaxL0 -> Count -> Maybe Count
    parent :: RangeMinMaxL0 -> Count -> Maybe Count
