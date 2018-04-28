module HaskellWorks.Data.RankSelect.ValidateSpec (spec) where

import Control.Monad
import Control.Monad.IO.Class
import Data.List                                 (isSuffixOf)
import Data.Maybe
import Data.Word
import GHC.Exts
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.BasicGen
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Take
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                                   hiding (drop, length, take)
import System.Directory
import System.IO.MMap
import System.IO.Unsafe
import Test.Hspec

import qualified Data.Vector.Storable      as DVS
import qualified HaskellWorks.Hedgehog.Gen as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

spec :: Spec
spec = describe "HaskellWorks.Data.RankSelect.ValidateSpec" $ do
  it "stub" $ requireProperty $ do
    True === True
