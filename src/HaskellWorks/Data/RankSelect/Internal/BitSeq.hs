{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module HaskellWorks.Data.RankSelect.Internal.BitSeq
  ( Elem(..)
  , Measure(..)
  , BitSeq(..)
  , BitSeqFt
  , (|>#)
  , (#<|)
  , ftSplit
  , atBitCountBelow
  , atPopCountBelow
  , splitAt
  ) where

import Control.DeepSeq
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FingerTree              (ViewL (..), ViewR (..), (<|), (><), (|>))
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import Prelude                                   hiding (max, min, splitAt)

import qualified HaskellWorks.Data.Cons       as HW
import qualified HaskellWorks.Data.Container  as HW
import qualified HaskellWorks.Data.FingerTree as FT
import qualified HaskellWorks.Data.Snoc       as HW

data Elem = Elem
  { elemBits :: {-# UNPACK #-} !Word64
  , elemSize :: {-# UNPACK #-} !Count
  } deriving (Eq, Show, Generic)

instance NFData Elem

data Measure = Measure
  { measureBitCount :: {-# UNPACK #-} !Count
  , measurePopCount :: {-# UNPACK #-} !Count
  } deriving (Eq, Ord, Show, Generic)

instance NFData Measure

type BitSeqFt = FT.FingerTree Measure Elem

newtype BitSeq = BitSeq
  { parens :: BitSeqFt
  } deriving (Show, NFData, Generic)

instance Semigroup Measure where
  a <> b = Measure
    { measureBitCount = measureBitCount a + measureBitCount b
    , measurePopCount = measurePopCount a + measurePopCount b
    }

instance Monoid Measure where
  mempty = Measure 0 0
  mappend = (<>)

instance FT.Measured Measure Elem where
  measure (Elem w size) = Measure
    { measureBitCount = size
    , measurePopCount = popCount1 w
    }

instance HW.Container BitSeq where
  type Elem BitSeq = Bool

instance HW.Cons BitSeq where
  cons b (BitSeq ft) = BitSeq $ case FT.viewl ft of
    Elem w nw :< rt -> if nw >= 0 && nw < 64
      then Elem ((w .<. 1) .|. bw) (nw + 1) <| rt
      else Elem bw 1                        <| ft
    FT.EmptyL        -> FT.singleton (Elem bw 1)
    where bw = if b then 1 else 0

instance HW.Snoc BitSeq where
  snoc (BitSeq ft) b = BitSeq $ case FT.viewr ft of
    lt :> Elem w nw -> if nw >= 0 && nw < 64
      then Elem (w .|. (bw .<. nw)) (nw + 1) <| lt
      else Elem bw 1                         <| lt
    FT.EmptyR        -> FT.singleton (Elem bw 1)
    where bw = if b then 1 else 0

instance Semigroup BitSeq where
  BitSeq tl <> BitSeq tr = BitSeq $ case FT.viewr tl of
    tll :> Elem wl nwl -> case FT.viewl tr of
      Elem wr nwr :< trr -> let nw = nwl + nwr in if nw <= 64
        then (tll |> Elem (wl .|. (wr .<. nwl)) nw) >< trr
        else tl >< tr
      FT.EmptyL -> tr
    FT.EmptyR -> FT.empty

instance Select1 BitSeq where
  select1 (BitSeq ft) n = case FT.split (atPopCountBelow n) ft of
    (lt, _) -> case FT.viewr lt of
      FT.EmptyR       -> 0
      llt :> Elem w _ ->
        let llpc = measurePopCount (FT.measure llt :: Measure) in
        let llbc = measureBitCount (FT.measure llt :: Measure) in
        llbc + select1 w (n - llpc)

instance Rank1 BitSeq where
  rank1 bs n = let (lt, _) = splitAt n bs in popCount1 lt

instance PopCount1 BitSeq where
  popCount1 (BitSeq ft) = measureBitCount (FT.measure ft :: Measure)

(|>#) :: BitSeqFt -> Elem -> BitSeqFt
(|>#) ft e@(Elem _ wn) = if wn > 0 then ft |> e else ft

(#<|) :: Elem ->BitSeqFt -> BitSeqFt
(#<|) e@(Elem _ wn) ft = if wn > 0 then e <| ft else ft

ftSplit :: (Measure -> Bool) -> BitSeqFt -> (BitSeqFt, BitSeqFt)
ftSplit p ft = case FT.viewl rt of
  Elem w nw :< rrt -> let c = go w nw nw in (lt |># Elem w c, Elem (w .>. c) (nw - c) #<| rrt)
  FT.EmptyL        -> (ft, FT.empty)
  where (lt, rt) = FT.split p ft
        ltm = FT.measure lt
        go :: Word64 -> Count -> Count -> Count
        go w c nw = if c > 0
          then if p (ltm <> FT.measure (Elem (w .<. (64 - c) .>. (64 - c)) c))
            then go w (c - 1) nw
            else c
          else 0

atBitCountBelow :: Count -> Measure -> Bool
atBitCountBelow n m = n < measureBitCount (m :: Measure)

atPopCountBelow :: Count -> Measure -> Bool
atPopCountBelow n m = n < measurePopCount (m :: Measure)

splitAt :: Count -> BitSeq -> (BitSeq, BitSeq)
splitAt n (BitSeq ft) = case FT.split (atBitCountBelow n) ft of
  (lt, rt) -> let
    n' = n - measureBitCount (FT.measure lt)
    u  = 64 - n'
    in case FT.viewl rt of
      Elem w nw :< rrt -> if n' >= nw
        then (BitSeq  lt                              , BitSeq                               rrt )
        else (BitSeq (lt |> Elem ((w .<. u) .>. u) n'), BitSeq (Elem (w .>. n') (nw - n') <| rrt))
      FT.EmptyL          -> (BitSeq lt, BitSeq FT.empty)
