{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Argon.Types (ComplexityBlock, AnalysisResult, Config(..)
                   , OutputMode(..))
    where

import Data.Aeson


-- | Hold the data associated to a function binding:
--   (line number, column, function name, complexity).
type ComplexityBlock = (Int, Int, String, Int)

instance ToJSON ComplexityBlock where
    toJSON (l, c, func, cc) = object [ "lineno"     .= l
                                     , "col"        .= c
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
