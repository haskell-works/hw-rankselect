
module HaskellWorks.Data.RankSelect.Gen where

import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.SpecCommon
import Hedgehog

import qualified Data.Vector.Storable as DVS
import qualified Hedgehog.Gen         as G

count :: MonadGen m => Range Count -> m Count
count = G.word64

vector :: (MonadGen m, DVS.Storable a) => Range Int -> m a -> m (ShowVector (DVS.Vector a))
vector r g = ShowVector . DVS.fromList <$> G.list r g
