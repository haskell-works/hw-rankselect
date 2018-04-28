module Test.Common where

import Data.List (isSuffixOf)

import qualified System.Directory as IO
import qualified System.IO.Unsafe as IO

corpusFiles :: [FilePath]
corpusFiles = IO.unsafePerformIO $ do
  entries <- IO.listDirectory "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return files
{-# NOINLINE corpusFiles #-}
