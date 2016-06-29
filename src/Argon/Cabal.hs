{-# LANGUAGE CPP #-}
module Argon.Cabal (parseExts, autoCabal)
    where

import Data.List (nub)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Safe (headMay)
import System.FilePath ((</>), takeExtension)
import qualified Language.Haskell.Extension            as Dist
import qualified Distribution.Verbosity                as Dist
import qualified Distribution.PackageDescription       as Dist
import qualified Distribution.PackageDescription.Parse as Dist

import Argon.Walker (allParents, safeListDirectory)


-- | Parse the given Cabal file generate a list of GHC extension flags. The
--   extension names are read from the default-extensions field in the library
--   section.
parseExts :: FilePath -> IO [String]
parseExts path = extract <$> Dist.readPackageDescription Dist.silent path
    where extract pkg = maybe [] extFromBI $
            (Dist.libBuildInfo . Dist.condTreeData) <$> Dist.condLibrary pkg

-- | From the current directory, look for a cabal file until one is found or
--   the root of the filesystem is hit.
autoCabal :: IO (Maybe FilePath)
autoCabal = headMay . concat <$> (mapM containsCabalFile =<< allParents)
    where containsCabalFile path = do
              contents <- safeListDirectory path
              return [path </> p | p <- contents, takeExtension p == ".cabal"]

extFromBI :: Dist.BuildInfo -> [String]
extFromBI binfo = map toString . nub $ allExts
    where toString (Dist.UnknownExtension ext) = ext
          toString (Dist.EnableExtension  ext) = show ext
          toString (Dist.DisableExtension ext) = show ext
          allExts = concatMap ($ binfo)
              [Dist.defaultExtensions, Dist.otherExtensions, Dist.oldExtensions]
