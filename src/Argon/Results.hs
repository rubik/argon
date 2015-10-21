module Argon.Results (order, filterResults, export)
    where

import Data.Ord (comparing)
import Data.List (sortBy)
import Argon.Formatters
import Argon.Types


-- sortOn is built-in only in base 4.8.0.0 onwards
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

-- | Order a list of blocks. Ordering is done with respect to:
--
--     1. complexity (descending)
--     2. line number (ascending)
--     3. function name (alphabetically)
order :: [ComplexityBlock] -> [ComplexityBlock]
order = sortOn (\(CC (l, _, f, cc)) -> (-cc, l, f))

-- | Filter the results of the analysis, with respect to the given
--   'ResultsOptions'.
filterResults :: Config
              -> (FilePath, AnalysisResult)
              -> (FilePath, AnalysisResult)
filterResults _ (s, Left msg) = (s, Left msg)
filterResults o (s, Right rs) =
    (s, Right $ order [r | r@(CC (_, _, _, cc)) <- rs, cc >= minCC o])

-- | Export analysis' results. How to export the data is defined by the
--   'ResultsOptions'.
export :: Config -> [(FilePath, AnalysisResult)] -> String
export opts rs =
    case outputMode opts of
        BareText -> bareTextFormatter rs
        Colored  -> coloredTextFormatter rs
        JSON     -> jsonFormatter rs
