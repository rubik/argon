{-# LANGUAGE CPP #-}
module Argon.Walker (allFiles)
    where

import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.List (foldl1', isSuffixOf)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mappend)
import Control.Applicative ((<$>))
#endif
import System.FilePath ((</>))
import System.Directory
import System.Directory.PathWalk


-- | Starting from a list of paths, generate a sequence of paths corresponding
--   to Haskell files. The fileystem is traversed in a manner similar to
--   reversed DFS.
allFiles :: [FilePath] -> IO (Seq FilePath)
allFiles paths = foldl1' mappend <$> mapM descend paths

descend :: FilePath -> IO (Seq FilePath)
descend path = do
    isFile <- doesFileExist path
    if isFile then return $ haskellFiles $ S.singleton path
              else findSourceFiles path

haskellFiles :: Seq FilePath -> Seq FilePath
haskellFiles = S.filter (".hs" `isSuffixOf`)

findSourceFiles :: FilePath -> IO (Seq FilePath)
findSourceFiles path =
    pathWalkAccumulate path $ \dir _ files ->
        return $ fmap (dir </>) $ haskellFiles $ S.fromList files
