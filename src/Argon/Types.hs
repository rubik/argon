{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Argon.Types (ComplexityBlock(CC), AnalysisResult, Config(..)
                   , OutputMode(..), GhcParseError(..))
    where

import Prelude hiding (span)
import Data.List (intercalate)
import Data.Aeson
import Data.Typeable
import Control.Exception (Exception)

import Argon.Span


data GhcParseError = GhcParseError {
    span :: Span
  , msg :: String
} deriving (Typeable)

-- | Hold the data associated to a function binding:
--   @(location, function name, complexity)@.
newtype ComplexityBlock = CC (Span, String, Int)
                        deriving (Show, Eq)

-- | Represent the result of the analysis of one file.
--   It can either be an error message or a list of
--   'ComplexityBlock's.
type AnalysisResult = Either String [ComplexityBlock]

-- | Type holding all the options passed from the command line.
data Config = Config {
    -- | Minimum complexity a block has to have to be shown in results.
    minCC :: Int
    -- | Describe how the results should be exported.
  , outputMode :: OutputMode
  }

-- | Type describing how the results should be exported.
data OutputMode = BareText -- ^ Text-only output, no colors.
                | Colored  -- ^ Text-only output, with colors.
                | JSON     -- ^ Data is serialized to JSON.

instance Exception GhcParseError

instance Show GhcParseError where
    show e = tagMsg (span e) $ fixNewlines (msg e)
        where fixNewlines = intercalate "\n\t\t" . lines

instance ToJSON ComplexityBlock where
    toJSON (CC ((s, c, e, _), func, cc)) =
        object [ "lineno"     .= s
               , "col"        .= c
               , "endline"    .= e
               , "name"       .= func
               , "complexity" .= cc
               ]

instance ToJSON (FilePath, AnalysisResult) where
    toJSON (p, Left err) = object [ "path"    .= p
                                  , "type"    .= ("error" :: String)
                                  , "message" .= err
                                  ]
    toJSON (p, Right rs) = object [ "path"   .= p
                                  , "type"   .= ("result" :: String)
                                  , "blocks" .= rs
                                  ]
