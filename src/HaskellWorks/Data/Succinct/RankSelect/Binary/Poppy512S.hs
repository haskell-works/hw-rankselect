module HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512S
    ( Poppy512S(..)
    , Rank1(..)
    , makePoppy512S
    , sampleRange
    ) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Search
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

data Poppy512S = Poppy512S
  { poppy512SBits   :: DVS.Vector Word64
  , poppy512Index   :: DVS.Vector Word64
  , poppy512Samples :: DVS.Vector Word64 -- Sampling position of each 8192 1-bit
  } deriving (Eq, Show)

popCount1Range :: (DVS.Storable a, PopCount1 a) => Int -> Int -> DVS.Vector a -> Count
popCount1Range start len = popCount1 . DVS.take len . DVS.drop start

makePoppy512S :: DVS.Vector Word64 -> Poppy512S
makePoppy512S v = Poppy512S
  { poppy512SBits     = v
  , poppy512Index = DVS.constructN (((DVS.length v +           8 - 1) `div`           8) + 1) gen512Index
  , poppy512Samples  = DVS.unfoldrN (fromIntegral (popCount1 v `div` 8192) + 1) genS (0, 0)
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

instance BitLength Poppy512S where
  bitLength v = vLength (poppy512SBits v) * bitLength (poppy512SBits v !!! 0)
  {-# INLINE bitLength #-}

instance TestBit Poppy512S where
  (.?.) = (.?.) . poppy512SBits
  {-# INLINE (.?.) #-}

instance BitRead Poppy512S where
  bitRead = fmap makePoppy512S . bitRead

instance Rank1 Poppy512S where
  rank1 (Poppy512S v i _) p =
    Count (i !!! toPosition (p `div` 512)) + rank1 (DVS.drop ((fromIntegral p `div` 512) * 8) v) (p `mod` 512)

instance Rank0 Poppy512S where
  rank0 (Poppy512S v i _) p =
    p `div` 512 * 512 - Count (i !!! toPosition (p `div` 512)) + rank0 (DVS.drop ((fromIntegral p `div` 512) * 8) v) (p `mod` 512)

sampleRange :: Poppy512S -> Count -> (Word64, Word64)
sampleRange (Poppy512S _ index samples) p =
  let j = (fromIntegral p - 1) `div` 8192 in
  if 0 <= j && j < DVS.length samples
    then  let pa = samples DVS.! j                in
          if j + 1 < DVS.length samples
            then  let pz = samples DVS.! (j + 1)          in
                  (pa, pz)
            else (pa, fromIntegral (DVS.length index - 1))
    else (1, fromIntegral (DVS.length index - 1))

instance Select1 Poppy512S where
  select1 iv@(Poppy512S v i _) p = if DVS.length v /= 0
      then toCount q * 512 + select1 (DVS.drop (fromIntegral q * 8) v) (p - s)
      else 0
    where q = binarySearch (fromIntegral p) wordAt iMin iMax
          s = Count (i !!! q)
          wordAt = (i !!!)
          (sampleMin, sampleMax) = sampleRange iv p
          iMin = fromIntegral $  (sampleMin - 1) `div` 512      :: Position
          iMax = fromIntegral $ ((sampleMax - 1) `div` 512) + 1 :: Position

instance OpenAt Poppy512S where
  openAt = openAt . poppy512SBits
  {-# INLINE openAt #-}

instance CloseAt Poppy512S where
  closeAt = closeAt . poppy512SBits
  {-# INLINE closeAt #-}

instance FindOpenN Poppy512S where
  findOpenN = findOpenN . poppy512SBits
  {-# INLINE findOpenN #-}

instance FindOpen Poppy512S where
  findOpen = findOpen . poppy512SBits
  {-# INLINE findOpen #-}

instance FindClose Poppy512S where
  findClose = findClose . poppy512SBits
  {-# INLINE findClose #-}

instance Enclose Poppy512S where
  enclose = enclose . poppy512SBits
  {-# INLINE enclose #-}

instance BalancedParens Poppy512S where
  firstChild  = firstChild  . poppy512SBits
  nextSibling = nextSibling . poppy512SBits
  parent      = parent      . poppy512SBits
  {-# INLINE firstChild  #-}
  {-# INLINE nextSibling #-}
  {-# INLINE parent      #-}
