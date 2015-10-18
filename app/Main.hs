module Main where

import System.Environment (getArgs)
import Argon (parseCode, formatResult)

readWithName :: String -> IO (Maybe String, String)
readWithName path = do
    contents <- readFile path
    return (Just path, contents)

main :: IO ()
main = getArgs >>= mapM readWithName
               >>= mapM_ (putStr . formatResult . uncurry parseCode)
