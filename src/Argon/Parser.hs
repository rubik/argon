module Argon.Parser (parseCode)
    where

import Data.Maybe (fromMaybe)
import Language.Haskell.Exts
import Language.Haskell.Exts.Parser (ParseMode(..), defaultParseMode
                                    , parseModuleWithMode
                                    , fromParseResult)
import Language.Preprocessor.Cpphs
import Argon.Visitor (ComplexityResult, funcsCC)


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

filenameMode :: String -> ParseMode
filenameMode fn = defaultParseMode {
                    parseFilename     = fn
                  , extensions        = customExts
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

parseCode :: Maybe String -> String -> (String, [ComplexityResult])
parseCode m = let fname = fromMaybe "<unknown>.hs" m
                in (,) fname . funcsCC . parseMModule fname

parseMModule :: String -> String -> Module
parseMModule fname = fromParseResult . parseModuleWithMode (filenameMode fname)
