{-# LANGUAGE QuasiQuotes #-}
module Main where

import System.Environment (getArgs)
import System.Console.Docopt
import Argon (parseCode, filterResults, formatResult)


patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

readWithName :: String -> IO (Maybe String, String)
readWithName path = do
    contents <- readFile path
    return (Just path, contents)

main :: IO ()
main = do
    args  <- parseArgsOrExit patterns =<< getArgs
    paths <- mapM readWithName $ args `getAllArgs` (argument "paths")
    let min = read $ getArgWithDefault args "1" $ longOption "min"
    mapM_ (putStr . formatResult . filterResults min . uncurry parseCode) paths
