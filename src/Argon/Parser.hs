module Argon.Parser (parseCode)
    where

import Data.Maybe (fromMaybe)
import Control.Exception (SomeException, evaluate, catch)
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Preprocessor.Cpphs
import Argon.Visitor (funcsCC)
import Argon.Types (AnalysisResult)


-- Very permissive extension set
customExts :: [Extension]
customExts = EnableExtension `map`
             [RecordWildCards, ScopedTypeVariables, CPP, MultiParamTypeClasses,
             TemplateHaskell,  RankNTypes, UndecidableInstances,
             FlexibleContexts, KindSignatures, EmptyDataDecls, BangPatterns,
             ForeignFunctionInterface, Generics, MagicHash, ViewPatterns,
             PatternGuards, TypeOperators, GADTs, PackageImports, MultiWayIf,
             SafeImports, ConstraintKinds, TypeFamilies, IncoherentInstances,
             FunctionalDependencies, ExistentialQuantification, ImplicitParams,
             UnicodeSyntax, LambdaCase, TupleSections, NamedFieldPuns]

argonMode :: ParseMode
argonMode = defaultParseMode {
                  extensions        = customExts
                , ignoreLinePragmas = False
                }

cppHsOpts :: CpphsOptions
cppHsOpts = defaultCpphsOptions {
                boolopts = defaultBoolOptions {
                             macros    = False
                           , stripEol  = True
                           , stripC89  = True
                           , pragma    = False
                           , hashline  = False
                           , locations = True
                           }
            }

handleExc:: (String -> ParseResult a) -> SomeException -> IO (ParseResult a)
handleExc helper = return . helper . show

-- | Parse the given code and compute cyclomatic complexity for every function
--   binding.
parseCode :: Maybe String -- ^ The filename of the source code
          -> String       -- ^ The source code
          -> IO (FilePath, AnalysisResult)
parseCode m source = do
    let fname = fromMaybe "<unknown>.hs" m
    parsed <- (do
        result <- parseModuleWithMode argonMode <$>
            runCpphs cppHsOpts fname source
        evaluate result) `catch` handleExc (ParseFailed noLoc)
    let res = case parsed of
                ParseOk moduleAst -> Right $ funcsCC moduleAst
                ParseFailed _ msg -> Left msg
    return (fname, res)
