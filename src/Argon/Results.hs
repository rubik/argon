module Argon.Results (filterResults)
    where

import Data.List (sortOn)
import Argon.Visitor (ComplexityResult)


order :: [ComplexityResult] -> [ComplexityResult]
order = sortOn (\(l, _, f, cc) -> (-cc, l, f))

filterResults :: Int -> (String, [ComplexityResult]) -> (String, [ComplexityResult])
filterResults m (s, rs) = (s, order [r | r@(_, _, _, cc) <- rs, cc >= m])
