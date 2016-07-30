module HaskellWorks.Data.Succinct.RankSelect.Binary.CsPoppy
    ( CsPoppy(..)
    , Rank1(..)
    , makeCsPoppy
    ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Search
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

data CsPoppy = CsPoppy
  { csPoppyBits     :: DVS.Vector Word64
  , csPoppy512Index :: DVS.Vector Word64
  , csPoppyLayer0   :: DVS.Vector Word64
  } deriving (Eq, Show)

makeCsPoppy :: DVS.Vector Word64 -> CsPoppy
makeCsPoppy v = CsPoppy
  { csPoppyBits     = v
  , csPoppy512Index = DVS.constructN (((DVS.length v +          8 - 1) `div`          8) + 1) gen512Index
  , csPoppyLayer0   = DVS.constructN (((DVS.length v + 0xffffffff - 1) `div` 0xffffffff) + 1) genLayer0
  }
  where gen512Index u = let indexN = DVS.length u - 1 in
          if indexN == -1
            then 0
            else getCount (popCount1 (DVS.take 8 (DVS.drop (indexN * 8) v))) + DVS.last u
        genLayer0 u = let indexN = DVS.length u - 1 in
          if indexN == -1
            then 0
            else getCount (popCount1 (DVS.take 0xffffffff (DVS.drop (indexN * 0xffffffff) v))) + DVS.last u

instance BitRead CsPoppy where
  bitRead = fmap makeCsPoppy . bitRead

instance Rank1 CsPoppy where
  rank1 (CsPoppy v i _) p =
    Count (i !!! toPosition (p `div` 512)) + rank1 (DVS.drop (fromIntegral p `div` 512) v) (p `mod` 512)

instance Rank0 CsPoppy where
  rank0 (CsPoppy v i _) p =
    p `div` 512 * 512 - Count (i !!! toPosition (p `div` 512)) + rank0 (DVS.drop (fromIntegral p `div` 512) v) (p `mod` 512)

instance Select1 CsPoppy where
  select1 (CsPoppy v i _) p = toCount q * 512 + select1 (DVS.drop (fromIntegral q * 8) v) (p - s)
    where q = binarySearch (fromIntegral p) wordAt 0 (fromIntegral $ DVS.length i - 1)
          s = Count (i !!! q)
          wordAt = (i !!!)

instance Select0 CsPoppy where
  select0 (CsPoppy v i _) p = toCount q * 512 + select0 (DVS.drop (fromIntegral q * 8) v) (p - s)
    where q = binarySearch (fromIntegral p) wordAt 0 (fromIntegral $ DVS.length i - 1)
          s = Count (fromIntegral q * 512 - (i !!! q))
          wordAt o = fromIntegral o * 512 - (i !!! o)
