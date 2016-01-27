-- The following code is taken and modified from ghc-exactprint, because adding
-- a dependency for just one module and then adding wrappers for that module
-- seemed excessive.
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module provides support for CPP and interpreter directives.
module Argon.Preprocess
   (
     CppOptions(..)
   , defaultCppOptions
   , getPreprocessedSrcDirect
   ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import qualified GHC
import qualified DynFlags       as GHC
import qualified MonadUtils     as GHC
import qualified DriverPhases   as GHC
import qualified DriverPipeline as GHC
import qualified HscTypes       as GHC

data CppOptions = CppOptions
                { cppDefine :: [String]    -- ^ CPP #define macros
                , cppInclude :: [FilePath] -- ^ CPP Includes directory
                , cppFile :: [FilePath]    -- ^ CPP pre-include file
                }


defaultCppOptions :: CppOptions
defaultCppOptions = CppOptions [] [] []

getPreprocessedSrcDirect :: (GHC.GhcMonad m)
                         => CppOptions
                         -> FilePath
                         -> m (String, GHC.DynFlags)
getPreprocessedSrcDirect cppOptions file = do
  hscEnv <- GHC.getSession
  let dfs = GHC.hsc_dflags hscEnv
      newEnv = hscEnv { GHC.hsc_dflags = injectCppOptions cppOptions dfs }
  (dflags', hspp_fn) <-
      GHC.liftIO $ GHC.preprocess newEnv (file, Just (GHC.Cpp GHC.HsSrcFile))
  txt <- GHC.liftIO $ readFile hspp_fn
  return (txt, dflags')

injectCppOptions :: CppOptions -> GHC.DynFlags -> GHC.DynFlags
injectCppOptions CppOptions{..} dflags =
  foldr addOptP dflags (map mkDefine cppDefine ++ map mkIncludeDir cppInclude
                                               ++ map mkInclude cppFile)
  where
    mkDefine     = ("-D" ++)
    mkIncludeDir = ("-I" ++)
    mkInclude    = ("-include" ++)

addOptP :: String -> GHC.DynFlags -> GHC.DynFlags
addOptP f = alterSettings (\s -> s { GHC.sOpt_P   = f : GHC.sOpt_P s})

alterSettings :: (GHC.Settings -> GHC.Settings) -> GHC.DynFlags -> GHC.DynFlags
alterSettings f dflags = dflags { GHC.settings = f (GHC.settings dflags) }
