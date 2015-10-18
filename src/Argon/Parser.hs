module Argon.Parser
    where

import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Parser (ParseMode(..), defaultParseMode
                                    , parseModuleWithMode
                                    , fromParseResult)
import Argon.Visitor (ComplexityResult, funcsCC)


filenameMode :: String -> ParseMode
filenameMode fn = defaultParseMode { parseFilename = fn }

parseCode :: Maybe String -> String -> (String, [ComplexityResult])
parseCode m = let fname = fromMaybe "<unknown>.hs" m
                in (,) fname . funcsCC . fromParseResult
                 . parseModuleWithMode (filenameMode fname)
