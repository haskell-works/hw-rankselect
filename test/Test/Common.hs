{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Common where

import Control.Exception
import Control.Monad.IO.Class
import Data.List              (isSuffixOf)
import Hedgehog

import qualified System.Directory as IO
import qualified System.IO.Unsafe as IO

corpusFiles :: [FilePath]
corpusFiles = IO.unsafePerformIO $ do
  entries <- IO.getDirectoryContents "data"
  let files = ("data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return files
{-# NOINLINE corpusFiles #-}

ioFailOnException :: a -> IO (Either String a)
ioFailOnException a = do
  catch (Right <$> evaluate a) handler
  where handler (e :: SomeException) = return (Left (show e))

safely :: a -> PropertyT IO a
safely a = do
  result <- liftIO $ ioFailOnException a
  case result of
    Right b -> return b
    Left msg -> do
      annotate msg
      failure
