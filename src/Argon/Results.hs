module Argon.Results (order, filterResults, export)
    where

import Data.List (sortOn)
import Argon.Formatters (bareTextFormatter, coloredTextFormatter)
import Argon.Types


order :: [ComplexityBlock] -> [ComplexityBlock]
order = sortOn (\(l, _, f, cc) -> (-cc, l, f))

-- | Filter the results of the analysis, keeping only those having a certain
--   complexity
filterResults :: ResultsOptions
              -> (FilePath, AnalysisResult)
              -> (FilePath, AnalysisResult)
filterResults _ (s, Left msg) = (s, Left msg)
filterResults o (s, Right rs) =
    (s, Right $ order [r | r@(_, _, _, cc) <- rs, cc >= minCC o])

export :: ResultsOptions -> (FilePath, AnalysisResult) -> String
export opts rs =
    case outputMode opts of
        BareText -> bareTextFormatter rs
        Colored  -> coloredTextFormatter rs
