{-# LANGUAGE LambdaCase #-}
module Argon.Formatters (bareTextFormatter, coloredTextFormatter)
    where

import Text.Printf (printf)
import System.Console.ANSI

import Pipes
import qualified Pipes.Prelude as P

import Argon.Types
import Argon.Loc


bareTextFormatter :: MonadIO m => Pipe (FilePath, AnalysisResult) String m ()
bareTextFormatter = formatResult
    id
    ("\terror: " ++)
    (\(CC (l, func, cc)) -> printf "\t%s %s - %d" (locToString l) func cc)

coloredTextFormatter :: MonadIO m => Pipe (FilePath, AnalysisResult) String m ()
coloredTextFormatter = formatResult
    (\name -> bold ++ name ++ reset)
    (printf "\t%serror%s: %s" (fore Red) reset)
    (\(CC (l, func, cc)) -> printf "\t%s %s - %s" (locToString l)
                                                  (coloredFunc func l)
                                                  (coloredRank cc))

-- | ANSI bold color
bold :: String
bold = setSGRCode [SetConsoleIntensity BoldIntensity]

-- | Make a ANSI foreground color sequence
fore :: Color -> String
fore color = setSGRCode [SetColor Foreground Dull color]

-- | ANSI sequence for reset
reset :: String
reset = setSGRCode []

coloredFunc :: String -> Loc -> String
coloredFunc f (_, c) = fore color ++ f ++ reset
    where color = if c == 1 then Cyan else Magenta

coloredRank :: Int -> String
coloredRank c = printf "%s%s (%d)%s" (fore color) rank c reset
    where (color, rank)
            | c <= 5    = (Green,  "A")
            | c <= 10   = (Yellow, "B")
            | otherwise = (Red,    "C")

formatResult :: (MonadIO m)
             => (String -> String)            -- ^ The header formatter
             -> (String -> String)            -- ^ The error formatter
             -> (ComplexityBlock -> String)   -- ^ The single line formatter
             -> Pipe (FilePath, AnalysisResult) String m ()
formatResult header errorF singleF = for cat $ \case
    (path, Left err) -> do
        yield $ header path
        yield $ errorF err
    (path, Right rs) -> do
        yield $ header path
        each rs >-> P.map singleF
