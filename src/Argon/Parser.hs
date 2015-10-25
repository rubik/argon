{-# LANGUAGE CPP #-}
module Argon.Parser (parseCode)
    where

import Control.Monad (void)
import qualified Control.Exception as E
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
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
import Argon.Utils

type LModule = GHC.Located (GHC.HsModule GHC.RdrName)


-- | Parse the code in the given filename and compute cyclomatic complexity for
--   every function binding.
parseCode :: FilePath  -- ^ The filename corresponding to the source code
          -> IO (FilePath, AnalysisResult)
parseCode file = do
    parseResult <- (do
        result <- parseModule file
        E.evaluate result) `E.catch` handleExc
    let analysis = case parseResult of
                      -- TODO: proper msg
                      Left (span, err) -> Left $ spanToString span ++ " " ++ err
                      Right ast        -> Right $ funcsCC ast
    return (file, analysis)

handleExc :: E.SomeException -> IO (Either (Span, String) LModule)
handleExc = return . Left . (,) (1,1,1,1) . show

parseModule :: FilePath -> IO (Either (Span, String) LModule)
parseModule = parseModuleWithCpp defaultCppOptions

-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp :: CppOptions
                   -> FilePath
                   -> IO (Either (Span, String) LModule)
parseModuleWithCpp cppOptions file =
    GHC.runGhc (Just libdir) $ do
      dflags <- initDynFlags file
      let useCpp = GHC.xopt GHC.Opt_Cpp dflags
      fileContents <-
        if useCpp
           then getPreprocessedSrcDirect cppOptions file
           else GHC.liftIO $ readFile file
      return $
        case parseFile dflags file fileContents of
          GHC.PFailed ss m -> Left (srcSpanToSpan ss, GHC.showSDoc dflags m)
          GHC.POk _ pmod   -> Right pmod

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult LModule
parseFile = runParser GHC.parseModule

runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where location   = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
          buffer     = GHC.stringToStringBuffer str
          parseState = GHC.mkPState flags buffer location

initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
    dflags0 <- GHC.getSessionDynFlags
    src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags0 file
    (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 src_opts
    let dflags2 = dflags1 { GHC.log_action = customLogAction }
    void $ GHC.setSessionDynFlags dflags2
    return dflags2

customLogAction :: GHC.LogAction
customLogAction dflags severity srcSpan _ msg =
    case severity of
      GHC.SevFatal -> fail $ GHC.showSDoc dflags msg  -- TODO: add srcspan
      _            -> return ()
