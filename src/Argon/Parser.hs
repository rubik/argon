module Argon.Parser
    where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Parser (ParseMode(..), defaultParseMode
                                    , parseModuleWithMode
                                    , fromParseResult)
import Argon.Visitor (ComplexityResult, funcsCC)


filenameMode :: String -> ParseMode
filenameMode fn = defaultParseMode { parseFilename = fn }

order :: [ComplexityResult] -> [ComplexityResult]
order = sortOn (\(l, _, f, cc) -> (-cc, l, f))

parseCode :: Maybe String -> String -> (String, [ComplexityResult])
parseCode m = let fname = fromMaybe "<unknown>.hs" m
                in (,) fname . order . funcsCC . fromParseResult
                 . parseModuleWithMode (filenameMode fname)
