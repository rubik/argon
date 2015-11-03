{-# LANGUAGE CPP #-}
module Argon.Parser (LModule, analyze, parseModule)
    where

import Data.List (foldl')
import Control.Monad (void)
import qualified Control.Exception as E

import qualified GHC hiding (parseModule)
import qualified SrcLoc       as GHC
import qualified Lexer        as GHC
import qualified Parser       as GHC
import qualified DynFlags     as GHC
import qualified HeaderInfo   as GHC
import qualified MonadUtils   as GHC
import qualified Outputable   as GHC
import qualified FastString   as GHC
import qualified StringBuffer as GHC
import GHC.Paths (libdir)

import Argon.Preprocess
import Argon.Visitor (funcsCC)
import Argon.Types
import Argon.Loc

-- | Type synonym for a syntax node representing a module tagged with a
--   'SrcSpan'
type LModule = GHC.Located (GHC.HsModule GHC.RdrName)


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

-- | Parse a module with the default instructions for the C pre-processor
-- | Only the includes directory is taken from the config
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
parseModuleWithCpp conf cppOptions file =
    GHC.runGhc (Just libdir) $ do
      dflags <- initDynFlags conf file
      let useCpp = GHC.xopt GHC.Opt_Cpp dflags
      (fileContents, dflags1) <-
        if useCpp
           then getPreprocessedSrcDirect cppOptions file
           else do
               contents <- GHC.liftIO $ readFile file
               return (contents, dflags)
      return $
        case parseCode dflags1 file fileContents of
          GHC.PFailed ss m -> Left $ tagMsg (srcSpanToLoc ss)
                                            (GHC.showSDoc dflags m)
          GHC.POk _ pmod   -> Right pmod

parseCode :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult LModule
parseCode = runParser GHC.parseModule

runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where location   = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
          buffer     = GHC.stringToStringBuffer str
          parseState = GHC.mkPState flags buffer location

initDynFlags :: GHC.GhcMonad m => Config -> FilePath -> m GHC.DynFlags
initDynFlags conf file = do
    dflags0 <- GHC.getSessionDynFlags
    src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags0 file
    (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 src_opts
    let cabalized = foldl' GHC.xopt_set dflags1 $ exts conf
    let dflags2 = cabalized { GHC.log_action = customLogAction }
    void $ GHC.setSessionDynFlags dflags2
    return dflags2

customLogAction :: GHC.LogAction
customLogAction dflags severity srcSpan _ m =
    case severity of
      GHC.SevFatal -> throwError
      GHC.SevError -> throwError
      _            -> return ()
    where throwError = E.throwIO $ GhcParseError (srcSpanToLoc srcSpan)
                                                 (GHC.showSDoc dflags m)
