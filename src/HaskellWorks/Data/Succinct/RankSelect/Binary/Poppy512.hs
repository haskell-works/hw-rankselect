module HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
    ( Poppy512(..)
    , Rank1(..)
    , makePoppy512
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

data Poppy512 = Poppy512
  { poppy512Bits  :: DVS.Vector Word64
  , poppy512Index :: DVS.Vector Word64
  } deriving (Eq, Show)

makePoppy512 :: DVS.Vector Word64 -> Poppy512
makePoppy512 v = Poppy512
  { poppy512Bits  = v
  , poppy512Index = DVS.constructN (((DVS.length v + 7) `div` 8) + 1) gen512Index
  }
  where gen512Index u = let indexN = DVS.length u - 1 in
          if indexN == -1
            then 0
            else getCount (popCount1 (DVS.take 8 (DVS.drop (indexN * 8) v))) + DVS.last u

instance BitRead Poppy512 where
  bitRead = fmap makePoppy512 . bitRead

instance Rank1 Poppy512 where
  rank1 (Poppy512 v i) p =
    Count (i !!! toPosition (p `div` 512)) + rank1 (DVS.drop (fromIntegral p `div` 512) v) (p `mod` 512)

instance Rank0 Poppy512 where
  rank0 (Poppy512 v i) p =
    p `div` 512 * 512 - Count (i !!! toPosition (p `div` 512)) + rank0 (DVS.drop (fromIntegral p `div` 512) v) (p `mod` 512)

instance Select1 Poppy512 where
  select1 (Poppy512 v i) p = toCount q * 512 + select1 (DVS.drop (fromIntegral q * 8) v) (p - s)
    where q = binarySearch (fromIntegral p) wordAt 0 (fromIntegral $ DVS.length i - 1)
          s = Count (i !!! q)
          wordAt = (i !!!)

instance Select0 Poppy512 where
  select0 (Poppy512 v i) p = toCount q * 512 + select0 (DVS.drop (fromIntegral q * 8) v) (p - s)
    where q = binarySearch (fromIntegral p) wordAt 0 (fromIntegral $ DVS.length i - 1)
          s = Count (fromIntegral q * 512 - (i !!! q))
          wordAt o = fromIntegral o * 512 - (i !!! o)
