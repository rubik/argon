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
--   to Haskell files. The filesystem is traversed depth-first.
allFiles :: (MonadIO m, MonadSafe m) => FilePath -> Producer FilePath m ()
allFiles path = do
    isFile <- liftIO $ doesFileExist path
    if isFile then each [path] >-> P.filter (".hs" `isSuffixOf`)
              else find path (glob "*.hs" <> regular)
