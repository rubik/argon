module Argon.Walker (allFiles)
    where

import           Control.Monad    (forM_, when)
import           Data.List        (isSuffixOf)
import           Pipes            (MonadIO, Producer, liftIO, yield)
import           System.Directory (doesDirectoryExist, doesFileExist,
                                   listDirectory, pathIsSymbolicLink)
import           System.FilePath  (takeExtension, (</>))

-- | Starting from a path, generate a sequence of paths corresponding
--   to Haskell files. The filesystem is traversed depth-first. Symbolic links
--   are not followed.
allFiles :: MonadIO m => FilePath -> Producer FilePath m ()
allFiles path = do
    isFile <- liftIO $ doesFileExist path
    if isFile then when (".hs" `isSuffixOf` path) $ yield path
              else walk path

-- | Recursively yield the @.hs@ files under a directory, depth-first.
walk :: MonadIO m => FilePath -> Producer FilePath m ()
walk dir = do
    entries <- liftIO $ listDirectory dir
    forM_ entries $ \e -> do
        let child = dir </> e
        isSymLink <- liftIO $ pathIsSymbolicLink child
        isDir <- liftIO $ doesDirectoryExist child
        if isDir && not isSymLink
           then walk child
           else when (not isSymLink && takeExtension child == ".hs") $
                    yield child
