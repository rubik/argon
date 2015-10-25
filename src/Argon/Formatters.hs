module Argon.Formatters (bareTextFormatter, coloredTextFormatter
                        , jsonFormatter)
    where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intercalate)
import Text.Printf (printf)
import System.Console.ANSI

import Argon.Types
import Argon.Span


bareTextFormatter :: [(FilePath, AnalysisResult)] -> String
bareTextFormatter = formatSingle $ formatResult
    (printf "%s\n\t%s")
    (\e -> "error:" ++ e)
    (\(CC (l, func, cc)) -> printf "%s %s - %d" (spanToString l) func cc)

coloredTextFormatter :: [(FilePath, AnalysisResult)] -> String
coloredTextFormatter = formatSingle $ formatResult
    (\name rest -> printf "%s%s%s\n\t%s%s" open name reset rest reset)
    (\e -> printf "%serror%s: %s%s" (fore Red) reset e reset)
    (\(CC (l, func, cc)) -> printf "%s %s - %s%s" (spanToString l)
                                                  (coloredFunc func l)
                                                  (coloredRank cc) reset)

jsonFormatter :: [(FilePath, AnalysisResult)] -> String
jsonFormatter = B.unpack . encode

open :: String
open = setSGRCode [SetConsoleIntensity BoldIntensity]

fore :: Color -> String
fore color = setSGRCode [SetColor Foreground Dull color]

reset :: String
reset = setSGRCode []

coloredFunc :: String -> Span -> String
coloredFunc f (_, c, _, _) = fore color ++ f ++ reset
    where color = if c == 1 then Cyan else Magenta

coloredRank :: Int -> String
coloredRank c = printf "%s%s (%d)%s" (fore color) rank c reset
    where (color, rank)
            | c <= 5    = (Green,  "A")
            | c <= 10   = (Yellow, "B")
            | otherwise = (Red,    "C")

formatSingle :: ((FilePath, AnalysisResult) -> String)
             -> [(FilePath, AnalysisResult)]
             -> String
formatSingle f = unlines . filter (not . null) . map f

formatResult :: (String -> String -> String)  -- ^ The block formatter
             -> (String -> String)            -- ^ The error formatter
             -> (ComplexityBlock -> String)   -- ^ The single line formatter
             -> (FilePath, AnalysisResult) -> String
formatResult resultBlock errorF _ (name, Left msg) =
    resultBlock name $ errorF msg
formatResult _ _ _ (_,    Right []) = ""
formatResult resultBlock _ singleF (name, Right rs) =
    resultBlock name $ intercalate "\n\t" $ map singleF rs
