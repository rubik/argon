{-# LANGUAGE TypeApplications #-}
module Argon.Parser (LModule, analyze, parseModule)
    where

import qualified Control.Exception as E
import Data.Either (fromRight)

import GHC.Hs                       (HsModule)
import GHC.Hs.Extension             (GhcPs)
import GHC.Types.SrcLoc             (Located, noLoc)
import GHC.Driver.Session           (DynFlags, defaultDynFlags, xopt
                                    , parseDynamicFlagsCmdLine)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Parser.Lexer             (ParseResult(POk, PFailed), PState
                                    , getPsErrorMessages)
import GHC.Types.Error              (getMessages, MsgEnvelope(..)
                                    , diagnosticMessage, defaultDiagnosticOpts
                                    , unDecorated)
import GHC.Parser.Errors.Types      (PsMessage)
import GHC.Utils.Outputable         (showSDocUnsafe)
import GHC.Data.Bag                 (bagToList)

import Language.Haskell.GhclibParserEx.GHC.Parser          (parseFile)
import Language.Haskell.GhclibParserEx.GHC.Driver.Session  (parsePragmasIntoDynFlags)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

import Argon.Preprocess
import Argon.Visitor (funcsCC)
import Argon.Types
import Argon.Loc

-- | Type synonym for a syntax node representing a module tagged with a
--   'SrcSpan'
type LModule = Located (HsModule GhcPs)


-- | Parse the code in the given filename and compute cyclomatic complexity for
--   every function binding.
analyze :: Config    -- ^ Configuration options
        -> FilePath  -- ^ The filename corresponding to the source code
        -> IO (FilePath, AnalysisResult)
analyze conf file = do
    parseResult <- (do
        result <- parseModule conf file
        E.evaluate result) `E.catch` handleExc
    let analysis = case parseResult of
                      Left err  -> Left err
                      Right ast -> Right $ funcsCC ast
    return (file, analysis)

handleExc :: E.SomeException -> IO (Either String LModule)
handleExc = return . Left . show

-- | Parse a module with the default instructions for the C pre-processor.
--   Only the includes directory is taken from the config.
parseModule :: Config -> FilePath -> IO (Either String LModule)
parseModule conf = parseModuleWithCpp conf $
    defaultCppOptions { cppInclude = includeDirs conf
                      , cppFile    = headers conf
                      }

-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp :: Config
                   -> CppOptions
                   -> FilePath
                   -> IO (Either String LModule)
parseModuleWithCpp conf cppOptions file = do
    raw     <- readFile file
    dflags1 <- initDynFlags conf
    -- Read the file's own LANGUAGE/OPTIONS pragmas (e.g. to learn whether CPP
    -- is enabled) before deciding whether to preprocess.
    dflags2 <- pragmaFlags dflags1 file raw
    (contents, dflags3) <-
        if xopt LangExt.Cpp dflags2
           then do pp <- runPreprocessor cppOptions file raw
                   -- Re-read pragmas: CPP may have revealed extensions that
                   -- were hidden inside #if blocks.
                   df <- pragmaFlags dflags2 file pp
                   return (pp, df)
           else return (raw, dflags2)
    return $
      case parseFile file dflags3 contents of
        PFailed pst  -> Left $ renderError pst
        POk _ pmod   -> Right pmod

-- | Base 'DynFlags' (no real GHC installation needed) with the configured
--   extensions enabled. Extensions are turned on via @-X@ flags so that names
--   like @"CPP"@ map to the right extension.
initDynFlags :: Config -> IO DynFlags
initDynFlags conf = do
    let dflags0 = defaultDynFlags fakeSettings
    (dflags1, _, _) <- parseDynamicFlagsCmdLine dflags0
        [noLoc ("-X" ++ e) | e <- exts conf]
    return dflags1

-- | Fold a source file's own pragmas into the given flags, ignoring pragma
--   parse failures (the main parse will surface any real problem).
pragmaFlags :: DynFlags -> FilePath -> String -> IO DynFlags
pragmaFlags dflags file src =
    fromRight dflags <$> parsePragmasIntoDynFlags dflags ([], []) file src

-- | Render the first parser error of a failed parse to a @line:col message@
--   string.
renderError :: PState -> String
renderError pst =
    case bagToList (getMessages (getPsErrorMessages pst)) of
      []      -> "parse error"
      (env:_) -> tagMsg (srcSpanToLoc (errMsgSpan env))
                        (renderDiagnostic (errMsgDiagnostic env))
  where
    renderDiagnostic :: PsMessage -> String
    renderDiagnostic = unwords . map showSDocUnsafe . unDecorated
                     . diagnosticMessage (defaultDiagnosticOpts @PsMessage)
