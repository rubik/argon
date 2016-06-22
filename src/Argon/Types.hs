{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif

module Argon.Types (ComplexityBlock(CC), AnalysisResult, Config(..)
                   , OutputMode(..), GhcParseError(..), defaultConfig)
    where

import Data.List (intercalate)
import Data.Aeson
import Data.Typeable
import Control.Exception (Exception)

import Argon.Loc


data GhcParseError = GhcParseError {
    loc :: Loc
  , msg :: String
} deriving (Typeable)

-- | Hold the data associated to a function binding:
--   @(location, function name, complexity)@.
newtype ComplexityBlock = CC (Loc, String, Int)
                        deriving (Show, Eq, Ord)

-- | Represent the result of the analysis of one file.
--   It can either be an error message or a list of
--   'ComplexityBlock's.
type AnalysisResult = Either String [ComplexityBlock]

-- | Type holding all the options passed from the command line.
data Config = Config {
    -- | Minimum complexity a block has to have to be shown in results.
    minCC       :: Int
    -- | Extension to activate
  , exts        :: [String]
    -- | Header files to be automatically included before preprocessing
  , headers     :: [FilePath]
    -- | Additional include directories for the C preprocessor
  , includeDirs :: [FilePath]
    -- | Describe how the results should be exported.
  , outputMode  :: OutputMode
  }

-- | Type describing how the results should be exported.
data OutputMode = BareText -- ^ Text-only output, no colors.
                | Colored  -- ^ Text-only output, with colors.
                | JSON     -- ^ Data is serialized to JSON.
                deriving (Show, Eq)

-- | Default configuration options.
--
--   __Warning__: These are not Argon's default options.
defaultConfig :: Config
defaultConfig = Config { minCC       = 1
                       , exts        = []
                       , headers     = []
                       , includeDirs = []
                       , outputMode  = JSON
                       }

instance Exception GhcParseError

instance Show GhcParseError where
    show e = tagMsg (loc e) $ fixNewlines (msg e)
        where fixNewlines = intercalate "\n\t\t" . lines

instance ToJSON ComplexityBlock where
    toJSON (CC ((s, c), func, cc)) =
        object [ "lineno"     .= s
               , "col"        .= c
               , "name"       .= func
               , "complexity" .= cc
               ]

instance {-# OVERLAPPING #-} ToJSON (FilePath, AnalysisResult) where
    toJSON (p, Left err) = object [ "path"    .= p
                                  , "type"    .= ("error" :: String)
                                  , "message" .= err
                                  ]
    toJSON (p, Right rs) = object [ "path"   .= p
                                  , "type"   .= ("result" :: String)
                                  , "blocks" .= rs
                                  ]
