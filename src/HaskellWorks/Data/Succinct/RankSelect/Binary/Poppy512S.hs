module HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512S
    ( Poppy512S(..)
    , Rank1(..)
    , makePoppy512S
    , sampleRange
    ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Search
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

data Poppy512S = Poppy512S
  { csPoppyBits     :: DVS.Vector Word64
  , csPoppy512Index :: DVS.Vector Word64
  , csPoppySamples  :: DVS.Vector Word64 -- Sampling position of each 8192 1-bit
  } deriving (Eq, Show)

popCount1Range :: (DVS.Storable a, PopCount1 a) => Int -> Int -> DVS.Vector a -> Count
popCount1Range start len = popCount1 . DVS.take len . DVS.drop start

makePoppy512S :: DVS.Vector Word64 -> Poppy512S
makePoppy512S v = Poppy512S
  { csPoppyBits     = v
  , csPoppy512Index = DVS.constructN (((DVS.length v +           8 - 1) `div`           8) + 1) gen512Index
  , csPoppySamples  = DVS.unfoldrN (fromIntegral (popCount1 v `div` 8192) + 1) genS (0, 0)
  }
  where gen512Index u = let indexN = DVS.length u - 1 in
          if indexN == -1
            then 0
            else getCount (popCount1Range (indexN *           8)           8 v) + DVS.last u
        genS :: (Count, Position) -> Maybe (Word64, (Count, Position))
        genS (pca, n) = if n < vEnd v
          then  let w = v !!! n in
                let pcz = pca + popCount1 w in
                if (8192 - 1 + pca) `div` 8192 /= (8192 - 1 + pcz) `div` 8192
                  then Just (fromIntegral n * 64 + fromIntegral (select1 w (fromIntegral (8192 - (pca `mod` 8192)))), (pcz, n + 1))
                  else genS (pcz, n + 1)
          else Nothing

instance BitRead Poppy512S where
  bitRead = fmap makePoppy512S . bitRead

instance Rank1 Poppy512S where
  rank1 (Poppy512S v i _) p =
    Count (i !!! toPosition (p `div` 512)) + rank1 (DVS.drop (fromIntegral p `div` 512) v) (p `mod` 512)

sampleRange :: Poppy512S -> Count -> (Word64, Word64)
sampleRange (Poppy512S v _ samples) p =
  let i = (fromIntegral p - 1) `div` 8192 in
  let pa = samples DVS.! i                  in
  let pz = samples DVS.! (i + 1)            in
  if i + 1 < DVS.length samples
    then (pa, pz)
    else (pa, fromIntegral (DVS.length v * 64))

instance Select1 Poppy512S where
  select1 (Poppy512S v i _) p = toCount q * 512 + select1 (DVS.drop (fromIntegral q * 8) v) (p - s)
    where q = binarySearch (fromIntegral p) wordAt 0 (fromIntegral $ DVS.length i - 1)
          s = Count (i !!! q)
          wordAt = (i !!!)
