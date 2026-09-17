{-# LANGUAGE CPP #-}

module Argon.Cabal (parseExts)
    where

import           Data.List                              (nub)

import qualified Distribution.PackageDescription        as Dist
import qualified Distribution.Simple.PackageDescription as Dist
import qualified Distribution.Verbosity                 as Dist
#if MIN_VERSION_Cabal(3,14,0)
import qualified Distribution.Utils.Path                 as Dist
#endif
import qualified Language.Haskell.Extension             as Dist


-- | Parse the given Cabal file generate a list of GHC extension flags. The
--   extension names are read from the default-extensions field in the library
--   section.
parseExts :: FilePath -> IO [String]
parseExts path = extract <$> readPackageDescription
    where
#if MIN_VERSION_Cabal(3,14,0)
          readPackageDescription = Dist.readGenericPackageDescription
              Dist.silent Nothing (Dist.makeSymbolicPath path)
#else
          readPackageDescription = Dist.readGenericPackageDescription Dist.silent path
#endif
          extract pkg = maybe []
            (extFromBI . Dist.libBuildInfo . Dist.condTreeData)
            (Dist.condLibrary pkg)

extFromBI :: Dist.BuildInfo -> [String]
extFromBI binfo = map toString . nub $ allExts
    where toString (Dist.UnknownExtension ext) = ext
          toString (Dist.EnableExtension  ext) = show ext
          toString (Dist.DisableExtension ext) = show ext
          allExts = concatMap ($ binfo)
              [Dist.defaultExtensions, Dist.otherExtensions, Dist.oldExtensions]
