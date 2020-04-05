{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.RankSelect.BitSeq
  ( BitSeq(..)
  , mempty
  , size
  , fromWord64s
  , fromPartialWord64s
  , toPartialWord64s
  , fromBools
  , toBools
  , BS.splitAt
  , take
  , drop
  , (<|), (><), (|>)
  , select1
  ) where

import Data.Coerce
import Data.Foldable
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FingerTree                 (ViewL (..), ViewR (..), (<|), (><), (|>))
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Select1    (select1)
import HaskellWorks.Data.RankSelect.Internal.BitSeq (BitSeq (BitSeq), BitSeqFt, Elem (Elem))
import Prelude                                      hiding (drop, max, min, splitAt, take)

import qualified Data.List                                    as L
import qualified HaskellWorks.Data.FingerTree                 as FT
import qualified HaskellWorks.Data.RankSelect.Internal.BitSeq as BS
import qualified HaskellWorks.Data.RankSelect.Internal.Word   as W

empty :: BitSeq
empty = BitSeq FT.empty

size :: BitSeq -> Count
size (BitSeq parens) = BS.measureBitCount (FT.measure parens)

-- TODO Needs optimisation
fromWord64s :: Traversable f => f Word64 -> BitSeq
fromWord64s = foldl go empty
  where go :: BitSeq -> Word64 -> BitSeq
        go ps w = BitSeq (BS.parens ps |> Elem w 64)

-- TODO Needs optimisation
fromPartialWord64s :: Traversable f => f (Word64, Count) -> BitSeq
fromPartialWord64s = foldl go empty
  where go :: BitSeq -> (Word64, Count) -> BitSeq
        go ps (w, n) = BitSeq (BS.parens ps |> Elem w n)

toPartialWord64s :: BitSeq -> [(Word64, Count)]
toPartialWord64s = L.unfoldr go . coerce
  where go :: BitSeqFt -> Maybe ((Word64, Count), BitSeqFt)
        go ft = case FT.viewl ft of
          BS.Elem w n :< rt -> Just ((w, coerce n), rt)
          FT.EmptyL         -> Nothing

fromBools :: [Bool] -> BitSeq
fromBools = go empty
  where go :: BitSeq -> [Bool] -> BitSeq
        go (BitSeq ps) (b:bs) = case FT.viewr ps of
          FT.EmptyR      -> go (BitSeq (FT.singleton (Elem b' 1))) bs
          lt :> Elem w n ->
            let newPs = if n >= 64
                then ps |> Elem b' 1
                else lt |> Elem (w .|. (b' .<. fromIntegral n)) (n + 1)
            in go (BitSeq newPs) bs
          where b' = if b then 1 else 0 :: Word64
        go ps [] = ps

toBools :: BitSeq -> [Bool]
toBools ps = toBoolsDiff ps []

toBoolsDiff :: BitSeq -> [Bool] -> [Bool]
toBoolsDiff ps = mconcat (fmap go (toPartialWord64s ps))
  where go :: (Word64, Count) -> [Bool] -> [Bool]
        go (w, n) = W.partialToBoolsDiff (fromIntegral n) w

drop :: Count -> BitSeq -> BitSeq
drop n ps = snd (BS.splitAt n ps)

take :: Count -> BitSeq -> BitSeq
take n ps = fst (BS.splitAt n ps)
