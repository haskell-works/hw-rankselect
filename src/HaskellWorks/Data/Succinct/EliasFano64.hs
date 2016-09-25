{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Succinct.EliasFano64
  ( EliasFano64(..)
  , FromEliasFano64(..)
  , ToEliasFano64(..)
  ) where

import qualified Data.Vector.Storable                 as DVS
import           Data.Word
import           HaskellWorks.Data.AtIndex
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.EliasFano64.Internal
import           HaskellWorks.Data.Bits.Log2
import           HaskellWorks.Data.Bits.PackedVector  as PV
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Take
import           Safe
import           Prelude hiding (length, take)

data EliasFano64 = EliasFano64
  { hi      :: DVS.Vector Word64
  , lo      :: PackedVector64
  , loBits  :: Int
  , count   :: Count
  } deriving (Eq, Show)

class FromEliasFano64 a where
  fromEliasFano64 :: EliasFano64 -> a

class ToEliasFano64 a where
  toEliasFano64 :: a -> EliasFano64

instance ToEliasFano64 [Word64] where
  toEliasFano64 ws = case lastMay ws of
    Just end' -> EliasFano64
      { hi      = DVS.fromList (packToWord64 (packToWord32 (packToWord16 (packToWord8 (mkHiBits loBits' ws)))))
      , lo      = PV.fromList loBits' ws
      , loBits  = fromIntegral loBits'
      , count   = length'
      }
      where length' = length ws
            loBits' = fromIntegral (log2 (end' `div` length')) :: Count
    Nothing -> EliasFano64
      { hi      = DVS.empty
      , lo      = PV.empty
      , loBits  = 0
      , count   = 0
      }

instance FromEliasFano64 [Word64] where
  fromEliasFano64 ef = gen `fmap` take (count ef) [0 ..]
    where gen :: Int -> Word64
          gen i = let pos             = (loBits ef * i)                                         in
                  let (index, offset) = pos `quotRem` 64                                        in
                  let loValue         = (lo ef !!! fromIntegral index) .>. fromIntegral offset  in
                  let hiValue         = (lo ef !!! fromIntegral index) .>. fromIntegral offset  in
                  loValue + hiValue

-- instance AtIndex EliasFano64 where
--   (!!!)   v i = v !! fromIntegral i
--   atIndex v i = v !! fromIntegral i
--   {-# INLINE (!!!)   #-}
--   {-# INLINE atIndex #-}
