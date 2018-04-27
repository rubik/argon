{-# LANGUAGE CPP #-}
module Argon.Cabal (parseExts)
    where

import           Data.List                             (nub)
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative                   ((<$>))
#endif

import qualified Distribution.PackageDescription       as Dist
import qualified Distribution.PackageDescription.Parse as Dist
import qualified Distribution.Verbosity                as Dist
import qualified Language.Haskell.Extension            as Dist


-- | Parse the given Cabal file generate a list of GHC extension flags. The
--   extension names are read from the default-extensions field in the library
--   section.
parseExts :: FilePath -> IO [String]
#if __GLASGOW_HASKELL__ < 802
parseExts path = extract <$> Dist.readPackageDescription Dist.silent path
#else
parseExts path = extract <$> Dist.readGenericPackageDescription Dist.silent path
#endif
    where extract pkg = maybe [] extFromBI $
            Dist.libBuildInfo . Dist.condTreeData <$> Dist.condLibrary pkg

extFromBI :: Dist.BuildInfo -> [String]
extFromBI binfo = map toString . nub $ allExts
    where toString (Dist.UnknownExtension ext) = ext
          toString (Dist.EnableExtension  ext) = show ext
          toString (Dist.DisableExtension ext) = show ext
          allExts = concatMap ($ binfo)
              [Dist.defaultExtensions, Dist.otherExtensions, Dist.oldExtensions]
