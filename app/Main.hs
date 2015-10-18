module Main where

import Control.Monad (mapM)
import System.Environment (getArgs)
import Argon (parseCode)

main :: IO ()
main = print . parseCode (Just path) =<< mapM readFile =<< getArgs
