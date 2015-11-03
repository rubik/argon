-- |
-- Module:      Argon
-- Copyright:   (c) 2015 Michele Lacchia
-- License:     ISC
-- Maintainer:  Michele Lacchia <michelelacchia@gmail.com>
-- Stability:   alpha
-- Portability: portable
--
-- Programmatic interface to Argon.
module Argon
    (
    -- * Types
      AnalysisResult
    , ComplexityBlock(CC)
    , OutputMode(..)
    , Config(..)
    , defaultConfig
    , Loc
    , LModule
    -- * Parsing
    , analyze
    , parseModule
    , parseExts
    -- * Manipulating results
    , order
    , filterResults
    , filterNulls
    , exportStream
    ) where

import Argon.Parser (LModule, analyze, parseModule)
import Argon.Results (order, filterResults, filterNulls, exportStream)
import Argon.Cabal (parseExts)
import Argon.Types
import Argon.Loc
