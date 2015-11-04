{-# LANGUAGE CPP #-}
module Argon.Cabal (flagsMap, parseExts)
    where

import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#else
import Control.Arrow ((&&&))
#endif

import qualified DynFlags as GHC
import qualified Language.Haskell.Extension            as Dist
import qualified Distribution.Verbosity                as Dist
import qualified Distribution.PackageDescription       as Dist
import qualified Distribution.PackageDescription.Parse as Dist


-- | A 'Map' from extensions names to extensions flags
flagsMap :: Map String GHC.ExtensionFlag
flagsMap = M.fromList $ map specToPair GHC.xFlags

-- | Parse the given Cabal file generate a list of GHC extension flags. The
--   extension names are read from the default-extensions field in the library
--   section.
parseExts :: FilePath -> IO [GHC.ExtensionFlag]
parseExts path = do
    pkg <- Dist.readPackageDescription Dist.silent path
    return . maybe [] extFromBI $ (Dist.libBuildInfo . Dist.condTreeData) <$> Dist.condLibrary pkg

extFromBI :: Dist.BuildInfo -> [GHC.ExtensionFlag]
extFromBI = mapMaybe (get . toString) . Dist.defaultExtensions
    where get = flip M.lookup flagsMap
          toString (Dist.UnknownExtension ext) = ext
          toString (Dist.EnableExtension  ext) = show ext
          toString (Dist.DisableExtension ext) = show ext

#if __GLASGOW_HASKELL__ < 710
specToPair :: (String, GHC.ExtensionFlag, a) -> (String, GHC.ExtensionFlag)
specToPair (a, b, _) = (a, b)
#else
specToPair :: GHC.FlagSpec GHC.ExtensionFlag -> (String, GHC.ExtensionFlag)
specToPair = GHC.flagSpecName &&& GHC.flagSpecFlag
#endif
