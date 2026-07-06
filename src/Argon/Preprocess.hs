{-# LANGUAGE RecordWildCards #-}
-- | This module provides support for the C pre-processor. Because
--   'ghc-lib-parser' does not ship GHC's driver pipeline, CPP is handled
--   in-process by the pure-Haskell 'cpphs' library instead of shelling out to
--   the system preprocessor.
module Argon.Preprocess
   (
     CppOptions(..)
   , defaultCppOptions
   , runPreprocessor
   ) where

import Language.Preprocessor.Cpphs
    ( CpphsOptions(..), BoolOptions(..)
    , defaultCpphsOptions, defaultBoolOptions, runCpphs )

data CppOptions = CppOptions
                { cppDefine :: [String]    -- ^ CPP #define macros
                , cppInclude :: [FilePath] -- ^ CPP Includes directory
                , cppFile :: [FilePath]    -- ^ CPP pre-include file
                }


defaultCppOptions :: CppOptions
defaultCppOptions = CppOptions [] [] []

-- | Run the C pre-processor over the given source contents. cpphs is told to
--   emit Haskell @{-\# LINE \#-}@ pragmas ('locations' on, 'hashline' off) so
--   that downstream parse locations still refer to the original source lines.
runPreprocessor :: CppOptions -> FilePath -> String -> IO String
runPreprocessor cppOptions = runCpphs (toCpphsOptions cppOptions)

toCpphsOptions :: CppOptions -> CpphsOptions
toCpphsOptions CppOptions{..} = defaultCpphsOptions
    { defines    = map parseDefine cppDefine
    , includes   = cppInclude
    , preInclude = cppFile
    , boolopts   = defaultBoolOptions { locations = True
                                      , hashline  = False
                                      , lang      = True
                                      }
    }
  where
    parseDefine d = case break (== '=') d of
                      (name, '=':val) -> (name, val)
                      (name, _)       -> (name, "1")
