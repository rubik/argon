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
    , ComplexityBlock
    , OutputMode(..)
    , ResultsOptions(..)
    -- * Parsing
    , parseCode
    -- * Manipulating results
    , order
    , filterResults
    , export
    ) where

import Argon.Parser (parseCode)
import Argon.Results (order, export, filterResults)
import Argon.Types
