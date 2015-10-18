module Main where

import System.Environment (getArgs)
import Argon (parseCode)

main :: IO ()
main = do
    (path:_) <- getArgs
    print . parseCode (Just path) =<< readFile path
