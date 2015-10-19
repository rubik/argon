{-# LANGUAGE QuasiQuotes #-}
module Main where

import System.Environment (getArgs)
import System.Console.Docopt
import Argon


patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

processFile :: String -> IO (FilePath, AnalysisResult)
processFile path = do
    contents <- readFile path
    parseCode (Just path) contents

main :: IO ()
main = do
    args  <- parseArgsOrExit patterns =<< getArgs
    res   <- mapM processFile $ args `getAllArgs` (argument "paths")
    let min = read $ getArgWithDefault args "1" $ longOption "min"
    mapM_ (putStr . formatResult . filterResults min) res
