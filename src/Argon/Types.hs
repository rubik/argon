module Argon.Types (ComplexityBlock, AnalysisResult, ResultsOptions(..)
                   , OutputMode(..))
    where


-- | Hold the data associated to a function binding:
--   (line number, column, function name, complexity)
type ComplexityBlock = (Int, Int, String, Int)

-- | Represent the result of the analysis of one file.
--   It can either be an error message or a list of
--   'ComplexityBlock's.
type AnalysisResult = Either String [ComplexityBlock]

data ResultsOptions = ResultsOptions {
    minCC :: Int
  , outputMode :: OutputMode
  }

data OutputMode = BareText | Colored
