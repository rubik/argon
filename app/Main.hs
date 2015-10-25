{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.List (foldl1', isSuffixOf)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Foldable (toList)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Directory
import System.Directory.PathWalk
import System.Console.Docopt
import Argon
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

getOpt :: Arguments -> String -> String -> String
getOpt args def opt = getArgWithDefault args def $ longOption opt

haskellFiles :: Seq FilePath -> Seq FilePath
haskellFiles = S.filter (".hs" `isSuffixOf`)

findSourceFiles :: FilePath -> IO (Seq FilePath)
findSourceFiles path =
    pathWalkAccumulate path $ \dir _ files ->
        return $ fmap (dir </>) $ haskellFiles $ S.fromList files

descend :: FilePath -> IO (Seq FilePath)
descend path = do
    isFile <- doesFileExist path
    if isFile then return $ haskellFiles $ S.singleton path
              else findSourceFiles path

allFiles :: [FilePath] -> IO (Seq FilePath)
allFiles paths = foldl1' mappend <$> mapM descend paths

readConfig :: Arguments -> Config
readConfig args =
    Config {
      minCC      = read $ getOpt args "1" "min"
    , outputMode = if args `isPresent` longOption "json"
                      then JSON
                      else if args `isPresent` longOption "no-color"
                              then BareText
                              else Colored
    }

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    ins  <- allFiles $ args `getAllArgs` argument "paths"
    res  <- mapM parseCode $ toList ins
    let conf = readConfig args
    putStr $ export conf $ map (filterResults conf) res
