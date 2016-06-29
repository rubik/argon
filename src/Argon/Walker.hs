{-# LANGUAGE CPP #-}
module Argon.Walker (allFiles, allParents, safeListDirectory)
    where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Pipes
import Pipes.Files
import Pipes.Safe (MonadSafe)
import qualified Pipes.Prelude as P
import Data.Monoid ((<>))
import Data.List (isSuffixOf)
import Control.Exception (SomeException, catch)
import System.FilePath (takeDirectory)
import System.Directory ( doesFileExist
                        , getCurrentDirectory
                        , getDirectoryContents
                        )


-- | Starting from a path, generate a sequence of paths corresponding
--   to Haskell files. The filesystem is traversed depth-first.
allFiles :: (MonadIO m, MonadSafe m) => FilePath -> Producer FilePath m ()
allFiles path = do
    isFile <- liftIO $ doesFileExist path
    if isFile then each [path] >-> P.filter (".hs" `isSuffixOf`)
              else find path (glob "*.hs" <> regular)

-- | Starting from the current directory, all parent paths are gathered into a
--   list.
allParents :: IO [FilePath]
allParents = go <$> getCurrentDirectory
    where go path
            | path == parent = [path]
            | otherwise   = path : go parent
                where parent = takeDirectory path

-- | List directory contents while ignoring all the exceptions
safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory path = catch (getDirectoryContents path) $
    \e -> let _ = (e :: SomeException) in return []
