module Argon.Walker (allFiles)
    where

import Pipes
import Pipes.Files
import Pipes.Safe
import qualified Pipes.Prelude as P
import Data.Monoid ((<>))
import Data.List (isSuffixOf)
import System.Directory (doesFileExist)


-- | Starting from a path, generate a sequence of paths corresponding
--   to Haskell files. The fileystem is traversed depth-first.
allFiles :: (MonadIO m, MonadSafe m) => FilePath -> IO (Producer FilePath m ())
allFiles path = do
    isFile <- doesFileExist path
    if isFile then return $ each [path] >-> P.filter (".hs" `isSuffixOf`)
              else return $ find path (glob "*.hs" <> regular)
