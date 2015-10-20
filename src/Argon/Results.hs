module Argon.Results (order, filterResults, export)
    where

import Data.List (sortOn)
import Argon.Formatters
import Argon.Types


-- | Order a list of blocks. Ordering is done with respect to:
--
--     1. complexity (descending)
--     2. line number (ascending)
--     3. function name (alphabetically)
order :: [ComplexityBlock] -> [ComplexityBlock]
order = sortOn (\(l, _, f, cc) -> (-cc, l, f))

-- | Filter the results of the analysis, with respect to the given
--   'ResultsOptions'
filterResults :: ResultsOptions
              -> (FilePath, AnalysisResult)
              -> (FilePath, AnalysisResult)
filterResults _ (s, Left msg) = (s, Left msg)
filterResults o (s, Right rs) =
    (s, Right $ order [r | r@(_, _, _, cc) <- rs, cc >= minCC o])

-- | Export analysis' results. How to export the data is defined by the
--   'ResultsOptions'.
export :: ResultsOptions -> [(FilePath, AnalysisResult)] -> String
export opts rs =
    case outputMode opts of
        BareText -> bareTextFormatter rs
        Colored  -> coloredTextFormatter rs
        JSON     -> jsonFormatter rs
