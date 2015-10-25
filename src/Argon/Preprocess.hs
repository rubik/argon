{-# LANGUAGE RecordWildCards #-}
-- | This module provides support for CPP and interpreter directives.
module Argon.Preprocess
   (
     CppOptions(..)
   , defaultCppOptions
   , getPreprocessedSrcDirect
   ) where

import qualified GHC
import qualified DynFlags       as GHC
import qualified MonadUtils     as GHC
import qualified StringBuffer   as GHC
import qualified DriverPipeline as GHC

data CppOptions = CppOptions
                { cppDefine :: [String]    -- ^ CPP #define macros
                , cppInclude :: [FilePath] -- ^ CPP Includes directory
                , cppFile :: [FilePath]    -- ^ CPP pre-include file
                }


defaultCppOptions :: CppOptions
defaultCppOptions = CppOptions [] [] []

getPreprocessedSrcDirect :: (GHC.GhcMonad m) => CppOptions -> FilePath -> m String
getPreprocessedSrcDirect cppOptions src =
    (\(a,_,_) -> a) <$> getPreprocessedSrcDirectPrim cppOptions src

getPreprocessedSrcDirectPrim :: (GHC.GhcMonad m)
                              => CppOptions
                              -> FilePath
                              -> m (String, GHC.StringBuffer, GHC.DynFlags)
getPreprocessedSrcDirectPrim cppOptions file = do
  hscEnv <- GHC.getSession
  let dfs = GHC.extractDynFlags hscEnv
      newEnv = GHC.replaceDynFlags hscEnv (injectCppOptions cppOptions dfs)
  (dflags', hspp_fn) <- GHC.liftIO $ GHC.preprocess newEnv (file, Nothing)
  buf <- GHC.liftIO $ GHC.hGetStringBuffer hspp_fn
  txt <- GHC.liftIO $ readFile hspp_fn
  return (txt, buf, dflags')

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
