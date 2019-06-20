module HaskellWorks.Data.RankSelect.Gen
  ( bitSeq
  ) where

import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.BitSeq (BitSeq)
import Hedgehog

import qualified HaskellWorks.Data.RankSelect.BitSeq as BS
import qualified Hedgehog.Gen                        as G

bools :: MonadGen m => Range Count -> m [Bool]
bools r = G.list (fmap fromIntegral r) G.bool

bitSeq :: MonadGen m => Range Count -> m BitSeq
bitSeq r = BS.fromBools <$> bools r
