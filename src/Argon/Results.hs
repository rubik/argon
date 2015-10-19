module Argon.Results (filterResults)
    where

import Data.List (sortOn)
import Argon.Types (AnalysisResult, ComplexityResult)


order :: [ComplexityResult] -> [ComplexityResult]
order = sortOn (\(l, _, f, cc) -> (-cc, l, f))

filterResults :: Int -> (FilePath, AnalysisResult) -> (FilePath, AnalysisResult)
filterResults _ (s, Left msg) = (s, Left msg)
filterResults m (s, Right rs) =
    (s, Right $ order [r | r@(_, _, _, cc) <- rs, cc >= m])
