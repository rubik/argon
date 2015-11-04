{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Pipes
import qualified Pipes.Prelude as P
import System.Environment (getArgs)
import System.Console.Docopt

import Argon


patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

getOpt :: Arguments -> String -> String -> String
getOpt args def opt = getArgWithDefault args def $ longOption opt

readConfig :: Arguments -> IO Config
readConfig args = do
    xFlags <- maybe (return []) parseExts $ getArg args $ longOption "cabal-file"
    return Config {
      minCC       = read $ getOpt args "1" "min"
    , exts        = xFlags
    , headers     = args `getAllArgs` longOption "cabal-macros"
    , includeDirs = args `getAllArgs` longOption "include-dir"
    , outputMode  = if args `isPresent` longOption "json"
                       then JSON
                       else if args `isPresent` longOption "no-color"
                               then BareText
                               else Colored
    }

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    ins  <- allFiles $ args `getAllArgs` argument "paths"
    conf <- readConfig args
    let source = each ins
              >-> P.mapM (analyze conf)
              >-> P.map (filterResults conf)
              >-> P.filter filterNulls
    runEffect $ exportStream conf source
