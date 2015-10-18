module Argon.Parser
    where

import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser (ParseMode(..), defaultParseMode
                                    , parseModuleWithMode
                                    , fromParseResult)
import Argon.Visitor (ComplexityResult, funCC)


filenameMode :: String -> ParseMode
filenameMode fn = defaultParseMode { parseFilename = fn }

moduleDecls :: Module -> [Decl]
moduleDecls (Module _ _ _ _ _ _ d) = d
moduleDecls _ = []

isFunBind :: Decl -> Bool
isFunBind (FunBind _) = True
isFunBind _           = False

parseCode :: Maybe String -> String -> [ComplexityResult]
parseCode m = map funCC . filter isFunBind . moduleDecls . fromParseResult
            . parseModuleWithMode (filenameMode $ fromMaybe "<unkown>.hs" m)
