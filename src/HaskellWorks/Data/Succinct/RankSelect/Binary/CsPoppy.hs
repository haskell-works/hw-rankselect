module HaskellWorks.Data.Succinct.RankSelect.Binary.CsPoppy
    ( CsPoppy(..)
    , Rank1(..)
    , makeCsPoppy
    ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Search
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

data CsPoppy = CsPoppy
  { csPoppyBits     :: DVS.Vector Word64
  , csPoppy512Index :: DVS.Vector Word64
  , csPoppyLayer0   :: DVS.Vector Word64
  , csPoppyLayer1   :: DVS.Vector Word64
  } deriving (Eq, Show)

popCount1Range :: (DVS.Storable a, PopCount1 a) => Int -> Int -> DVS.Vector a -> Count
popCount1Range start len = popCount1 . DVS.take len . DVS.drop start

makeCsPoppy :: DVS.Vector Word64 -> CsPoppy
makeCsPoppy v = CsPoppy
  { csPoppyBits     = v
  , csPoppy512Index = DVS.constructN (((DVS.length v +           8 - 1) `div`           8) + 1) gen512Index
  , csPoppyLayer0   = DVS.constructN (((DVS.length v + 0x100000000 - 1) `div` 0x100000000) + 1) genLayer0
  , csPoppyLayer1   = DVS.constructN (((DVS.length v +          32 - 1) `div`          32) + 1) genLayer1
  }
  where csPoppyCum2048  = DVS.constructN (((DVS.length v +          32 - 1) `div`          32) + 1) genCum2048
        gen512Index u = let indexN = DVS.length u - 1 in
          if indexN == -1
            then 0
            else getCount (popCount1Range (indexN *           8)           8 v) + DVS.last u
        genCum2048 u = let indexN = DVS.length u in
          if indexN .&. 0xffffffff == 0
            then 0
            else getCount (popCount1Range ((indexN - 1) *    32)          32 v) + DVS.last u
        genLayer0 u = let indexN = DVS.length u in
          if indexN == 0
            then 0
            else getCount (popCount1Range (indexN * 0x100000000) 0x100000000 v) + DVS.last u
        genLayer1 u = let indexN = DVS.length u in
          let cum = if indexN == 0 -- TODO Check boundary at 4G???
              then  0
              else  csPoppyCum2048 !!! fromIntegral indexN in
          let a = getCount (popCount1Range (indexN * 32 +  0)  8 v) in
          let b = getCount (popCount1Range (indexN * 32 +  8)  8 v) in
          let c = getCount (popCount1Range (indexN * 32 + 16)  8 v) in
          (   ( cum       .&. 0x00000000ffffffff)
          .|. ((a .<. 32) .&. 0x000003ff00000000)
          .|. ((b .<. 42) .&. 0x000ffc0000000000)
          .|. ((c .<. 52) .&. 0x3ff0000000000000)) -- zhou-sea2013 fig 5 (c)

instance BitRead CsPoppy where
  bitRead = fmap makeCsPoppy . bitRead

instance Rank1 CsPoppy where
  rank1 (CsPoppy v _ layer0 layer1) p = rankPrior + rankInBasicBlock
    where rankLayer0              = layer0  !!! toPosition (p `div` 0x100000000)
          rankLayer1Word          = layer1  !!! toPosition (p `div` 2048)
          rankLayer1A             =  rankLayer1Word .&. 0x00000000ffffffff
          rankLayer1B             = (rankLayer1Word .&. 0x000003ff00000000) .>. 32
          rankLayer1C             = (rankLayer1Word .&. 0x000ffc0000000000) .>. 42
          rankLayer1D             = (rankLayer1Word .&. 0x3ff0000000000000) .>. 52
          q                       = (p `div` 512) `mod` 4 -- quarter
          rankLayer1  | q == 0    = rankLayer1A
                      | q == 1    = rankLayer1A + rankLayer1B
                      | q == 2    = rankLayer1A + rankLayer1B + rankLayer1C
                      | q == 3    = rankLayer1A + rankLayer1B + rankLayer1C + rankLayer1D
                      | otherwise = undefined
          rankPrior               = Count (rankLayer0 + rankLayer1)
          rankInBasicBlock        = rank1 (DVS.drop (fromIntegral p `div` 512) v) (p `mod` 512)

instance Select1 CsPoppy where
  select1 (CsPoppy v i _ _) p = toCount q * 512 + select1 (DVS.drop (fromIntegral q * 8) v) (p - s)
    where q = binarySearch (fromIntegral p) wordAt 0 (fromIntegral $ DVS.length i - 1)
          s = Count (i !!! q)
          wordAt = (i !!!)
