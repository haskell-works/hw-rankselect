{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Validate
  ( cmdValidate
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Generics.Product.Any
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import Options.Applicative

import qualified App.Commands.Options.Type            as Z
import qualified Data.Vector.Storable                 as DVS
import qualified HaskellWorks.Data.FromForeignRegion  as IO
import qualified HaskellWorks.Data.RankSelect.CsPoppy as CS
import qualified System.IO                            as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

runValidate :: Z.ValidateOptions -> IO ()
runValidate opts = case opts ^. the @"indexType" of
  Z.CsPoppy  -> do
    !(v :: DVS.Vector Word64) <- IO.mmapFromForeignRegion (opts ^. the @"file")

    let !csIndex = CS.makeCsPoppy v

    (_, final) <- flip runStateT Z.emptyValidateState $
      forM_ [0 .. fromIntegral (DVS.length v - 1)] $ \i -> do
        let w = v !!! i
        let wPopCount = popCount1 w
        lastPopCount <- use (the @"cumulativePopCount")

        forM_ [0 .. 64] $ \pw -> do
          let r = rank1 w pw + lastPopCount
          let p = fromIntegral (i * 64) + pw
          let actualR = rank1 csIndex p
          when (actualR /= r) $ do
            liftIO $ IO.putStrLn $ "Corrupt bit-index.  Expected: rank1 " <> show p <> " == " <> show r <> ", but: rank1 " <> show p <> " == " <> show actualR

        forM_ [0 .. 63] $ \pw -> do
          let r0 = rank1 w  pw      + lastPopCount
          let r1 = rank1 w (pw + 1) + lastPopCount
          let p = fromIntegral (i * 64) + pw
          when (r0 == r1) $ do
            let actualP = select1 csIndex r0
            when (actualP > p) $ do
              liftIO $ IO.putStrLn $ "Corrupt bit-index.  Expected: select1 " <> show r0 <> " <= " <> show p <> ", but: select1 " <> show r0 <> " == " <> show actualP

        forM_ [lastPopCount + 1 .. lastPopCount + wPopCount] $ \r -> do
          let p = select1 csIndex r
          when (rank1 csIndex p /= r) $ do
            liftIO $ IO.putStrLn $ "Corrupt bit-index at (p, r) = (" <> show p <> ", " <> show r <> ")"
            return ()
          return ()

        the @"cumulativePopCount" += wPopCount

        return ()

    IO.putStrLn $ "PopCounts validated: " <> show (final ^. the @"cumulativePopCount")

    return ()
  Z.Poppy512 -> do
    putStrLn "Not implemented"
    return ()

optsValidate :: Parser Z.ValidateOptions
optsValidate = Z.ValidateOptions
  <$> option auto
      (   long "index-type"
      <>  help "Index type"
      <>  metavar "INDEX_TYPE"
      )
  <*> strOption
      (   long "file"
      <>  help "Input file"
      <>  metavar "FILE"
      )

cmdValidate :: Mod CommandFields (IO ())
cmdValidate = command "validate"  $ flip info idm $ runValidate <$> optsValidate
