{-# LANGUAGE QuasiQuotes #-}
module Main where

import System.Environment (getArgs)
import System.Console.Docopt
import Argon


patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

getOpt :: Arguments -> String -> String -> String
getOpt args def opt = getArgWithDefault args def $ longOption opt

processFile :: String -> IO (FilePath, AnalysisResult)
processFile path = do
    contents <- readFile path
    parseCode (Just path) contents

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    res  <- mapM processFile $ args `getAllArgs` argument "paths"
    let opts = ResultsOptions {
        minCC      = read $ getOpt args "1" "min"
      , outputMode = if args `isPresent` longOption "json"
                        then JSON
                        else if args `isPresent` longOption "no-color"
                                then BareText
                                else Colored
      }
    putStr $ export opts $ map (filterResults opts) res
