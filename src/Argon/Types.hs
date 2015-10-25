{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Argon.Types (Span, ComplexityBlock(CC), AnalysisResult, Config(..)
                   , OutputMode(..))
    where

import Data.Aeson


type Span = (Int, Int, Int, Int)

-- | Hold the data associated to a function binding:
--   (location, function name, complexity).
newtype ComplexityBlock = CC (Span, String, Int)
                        deriving (Show, Eq)

instance ToJSON ComplexityBlock where
    toJSON (CC ((s, c, e, _), func, cc)) =
        object [ "lineno"     .= s
               , "col"        .= c
               , "endline"    .= e
               , "name"       .= func
               , "complexity" .= cc
               ]

-- | Represent the result of the analysis of one file.
--   It can either be an error message or a list of
--   'ComplexityBlock's.
type AnalysisResult = Either String [ComplexityBlock]

instance ToJSON (FilePath, AnalysisResult) where
    toJSON (p, Left err) = object [ "path"    .= p
                                  , "type"    .= ("error" :: String)
                                  , "message" .= err
                                  ]
    toJSON (p, Right rs) = object [ "path"   .= p
                                  , "type"   .= ("result" :: String)
                                  , "blocks" .= rs
                                  ]

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
