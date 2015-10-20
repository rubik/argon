module Argon.Formatters (bareTextFormatter, coloredTextFormatter)
    where

import Text.Printf (printf)
import Data.List (intercalate)
import System.Console.ANSI
import Argon.Types


bareTextFormatter :: (FilePath, AnalysisResult) -> String
bareTextFormatter = formatResult
    (printf "%s\n\t%s\n")
    (\e -> "error:" ++ e)
    (\(l, c, func, cc) -> printf "%d:%d %s - %d" l c func cc)

coloredTextFormatter :: (FilePath, AnalysisResult) -> String
coloredTextFormatter = formatResult
    (\name rest -> printf "%s%s%s\n\t%s\n%s" open name reset rest reset)
    (\e -> printf "%serror%s: %s%s" (fore Red) reset e reset)
    (\(l, c, func, cc) -> printf "%d:%d %s - %s%s" l c (coloredFunc func c)
                            (coloredRank cc) reset)

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

formatResult :: (String -> String -> String)  -- ^ The block formatter
             -> (String -> String)            -- ^ The error formatter
             -> (ComplexityBlock -> String)   -- ^ The single line formatter
             -> (FilePath, AnalysisResult) -> String
formatResult resultBlock errorF _ (name, Left msg) =
    resultBlock name $ errorF msg
formatResult _ _ _ (_,    Right []) = ""
formatResult resultBlock _ singleF (name, Right rs) =
    resultBlock name $ intercalate "\n\t" $ map singleF rs
