module Argon.Pretty (formatResult)
    where

import Text.Printf (printf)
import Data.List (intersperse)
import System.Console.ANSI
import Argon.Types (AnalysisResult)


open :: String
open = setSGRCode [SetConsoleIntensity BoldIntensity]

fore :: Color -> String
fore color = setSGRCode [SetColor Foreground Dull color]

reset :: String
reset = setSGRCode []

coloredFunc :: String -> Int -> String
coloredFunc f c = fore color ++ f ++ reset
    where color = if c == 1 then Cyan else Magenta

coloredRank :: Int -> String
coloredRank c = printf "%s%s (%d)%s" (fore color) rank c reset
    where (color, rank)
            | c <= 5    = (Green,  "A")
            | c <= 10   = (Yellow, "B")
            | otherwise = (Red,    "C")

coloredError :: String -> String
coloredError msg = printf "%serror%s: %s" (fore Red) reset msg

resultBlock :: String -> String -> String
resultBlock name rest = printf "%s%s%s\n\t%s\n%s" open name reset rest reset

formatResult :: (FilePath, AnalysisResult) -> String
formatResult (name, Left msg) = resultBlock name $ coloredError msg
formatResult (_,    Right []) = ""
formatResult (name, Right rs) = resultBlock name rest
    where rest     = concat (intersperse "\n\t" $ map single rs)
          single (l, c, func, cc) =
              printf "%d:%d %s - %s%s" l c (coloredFunc func c)
                                       (coloredRank cc) reset
