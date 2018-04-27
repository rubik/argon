{-# LANGUAGE OverloadedStrings #-}
module Argon.Walker (allFiles)
    where

import           Data.DirStream            (childOf)
import           Data.List                 (isSuffixOf)
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           Pipes                     (ListT, MonadIO, Producer, each,
                                            every, liftIO, (>->))
import qualified Pipes.Prelude             as P
import           Pipes.Safe
import           System.Directory          (doesDirectoryExist, doesFileExist,
                                            pathIsSymbolicLink)
import           System.FilePath           (takeExtension)

-- | Starting from a path, generate a sequence of paths corresponding
--   to Haskell files. The filesystem is traversed depth-first.
allFiles :: (MonadIO m, MonadSafe m) => FilePath -> Producer FilePath m ()
allFiles path = do
    isFile <- liftIO $ doesFileExist path
    if isFile then each [path] >-> P.filter (".hs" `isSuffixOf`)
              else every $ hsFilesIn path
                    -- find path (glob "*.hs" <> regular)
                   -- TODO: reimplement this, or the whole "allFiles" function.

-- | List the regular files in a directory.
hsFilesIn :: MonadSafe m => FilePath -> ListT m FilePath
hsFilesIn path = do
  child <- encodeString <$> childOf (decodeString path)
  isDir <- liftIO $ doesDirectoryExist child
  isSymLink <- liftIO $ pathIsSymbolicLink child
  if isDir && not isSymLink
    then hsFilesIn child
    else if not isSymLink && takeExtension child == ".hs"
         then return child
         else mempty
