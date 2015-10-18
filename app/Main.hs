module Main where

import System.Environment (getArgs)
import Argon (parseCode)

readWithName :: String -> IO (Maybe String, String)
readWithName path = do
    contents <- readFile path
    return (Just path, contents)

main :: IO ()
main = getArgs >>= mapM readWithName >>= print . map (uncurry parseCode)
