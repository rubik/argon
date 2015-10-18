module Argon.Pretty (formatResult)
    where

import Text.Printf (printf)
import Data.List (intersperse)
import Argon.Visitor (ComplexityResult)


formatResult :: (String, [ComplexityResult]) -> String
formatResult (name, rs) = name ++ "\n\t" ++ rest
    where rest   = concat (intersperse "\n\t" $ map single rs)
          single (func, l, c, cc) = printf "%d:%d %s - %d" func l c cc
