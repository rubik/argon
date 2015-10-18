module Argon.Pretty (formatResult)
    where

import Text.Printf (printf)
import Data.List (intersperse)
import System.Console.ANSI
import Argon.Visitor (ComplexityResult)


open :: String
open = setSGRCode [SetConsoleIntensity BoldIntensity]

fore :: Color -> String
fore color = setSGRCode [SetColor Foreground Dull color]

reset :: String
reset = setSGRCode []

coloredFunc :: String -> String
coloredFunc f = fore Cyan ++ f ++ reset

coloredRank :: Int -> String
coloredRank c = printf "%s%s (%d)%s" (fore color) rank c reset
    where (color, rank)
            | c <= 5    = (Green,  "A")
            | c <= 10   = (Yellow, "B")
            | otherwise = (Red,    "C")

formatResult :: (String, [ComplexityResult]) -> String
formatResult (_, [])    = ""
formatResult (name, rs) = printf "%s%s%s\n\t%s\n%s" open name reset rest reset
    where rest     = concat (intersperse "\n\t" $ map single rs)
          single (l, c, func, cc) =
              printf "%d:%d %s - %s%s" l c (coloredFunc func) (coloredRank cc) reset
