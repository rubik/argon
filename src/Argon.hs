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
    -- * Gathering source files
    , allFiles
    -- * Parsing
    , analyze
    , parseModule
    , parseExts
    -- * Manipulating results
    , order
    , filterResults
    , filterNulls
    , exportStream
    -- * Formatting results
    , bareTextFormatter
    , coloredTextFormatter
    -- * Utilities
    , srcSpanToLoc
    , locToString
    , tagMsg
    ) where

import Argon.Parser (LModule, analyze, parseModule)
import Argon.Results (order, filterResults, filterNulls, exportStream)
import Argon.Cabal (parseExts)
import Argon.Types
import Argon.Loc
import Argon.Walker (allFiles)
import Argon.Formatters (bareTextFormatter, coloredTextFormatter)
