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

-- | Type holding all the options passed from the command line
data ResultsOptions = ResultsOptions {
    -- | Minimum complexity a block has to have to be shown in results
    minCC :: Int
    -- | Describe how the results should be exported
  , outputMode :: OutputMode
  }

-- | Type describing how the results should be exported
data OutputMode = BareText -- ^ Text-only output, no colors
                | Colored  -- ^ Text-only output, with colors
