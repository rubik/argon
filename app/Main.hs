{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Data.List (foldl1', isSuffixOf)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mappend)
import Control.Applicative ((<$>))
#endif
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Pipes
import Pipes.Parse (Parser, draw, evalStateT)
import qualified Pipes.Prelude as P
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Directory
import System.Directory.PathWalk
import System.Console.Docopt

import Argon


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

filterNulls :: (FilePath, AnalysisResult) -> Bool
filterNulls (_, r) = case r of
                       Left  _  -> True
                       Right [] -> False
                       _        -> True

jsonParser :: Parser (FilePath, AnalysisResult) IO ()
jsonParser = do
    lift $ putChar '['
    first <- draw
    case first of
      Nothing -> lift $ putChar ']'
      Just el -> do
          lift $ B.putStr $ encode el
          let loop = do
                mx <- draw
                case mx of
                  Nothing -> lift $ putChar ']'
                  Just x  -> lift (putChar ',' *> B.putStr (encode x)) *> loop
          loop

textParser :: Config -> Parser (FilePath, AnalysisResult) IO ()
textParser conf = do
    first <- draw
    case first of
      Nothing -> return ()
      Just el -> lift (putStrLn $ export conf el) *> textParser conf

exportStream :: Config -> Parser (FilePath, AnalysisResult) IO ()
exportStream conf =
    case outputMode conf of
      JSON -> jsonParser
      _    -> textParser conf

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    ins  <- allFiles $ args `getAllArgs` argument "paths"
    let conf = readConfig args
        pipeline = each ins
                >-> P.mapM analyze
                >-> P.map (filterResults conf)
                >-> P.filter filterNulls
    evalStateT (exportStream conf) pipeline
