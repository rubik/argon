{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module ArgonSpec (spec)
    where

import Data.List (sort, isPrefixOf)
import Data.Aeson (encode)
import Text.Printf (printf)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Test.Hspec
import Test.QuickCheck
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Console.ANSI
import qualified SrcLoc     as GHC
import qualified FastString as GHC
import Pipes
--import Pipes.Safe (SafeT, runSafeT)
import qualified Pipes.Prelude as P

import Argon

instance Arbitrary ComplexityBlock where
    arbitrary = (\a b c -> CC (a, b, c)) <$> arbitrary
                                         <*> arbitrary
                                         <*> arbitrary
    shrink (CC t) = map CC $ shrink t

instance Arbitrary OutputMode where


ones :: Loc
ones = (1, 1)

lo :: Int -> Loc
lo s = (s, 1)

realSpan :: Int -> Int -> GHC.SrcSpan
realSpan a b = GHC.mkSrcSpan (mkLoc a b) $ mkLoc (-a) (b + 24)
    where mkLoc = GHC.mkSrcLoc (GHC.mkFastString "real loc")

errStartsWith :: String -> (FilePath, AnalysisResult) -> Bool
errStartsWith _ (_, Right _)  = False
errStartsWith p (_, Left msg) = p `isPrefixOf` msg

path :: String -> FilePath
path f = "test" </> "data" </> f

shouldAnalyze :: String -> AnalysisResult -> Expectation
shouldAnalyze f = shouldAnalyzeC (f, defaultConfig)

shouldAnalyzeC :: (String, Config) -> AnalysisResult -> Expectation
shouldAnalyzeC (f, config) r = analyze config p `shouldReturn` (p, r)
    where p = path f

-- Disabled until I figure out why Argon.Walker tests fail only on Travis
{-shouldProduceS :: Producer FilePath (SafeT IO) () -> [FilePath] -> Expectation-}
{-shouldProduceS prod res = do-}
    {-paths <- runSafeT $ P.toListM prod-}
    {-paths `shouldBe` res-}

shouldProduce :: (Eq a, Show a) => Producer a IO () -> [a] -> Expectation
shouldProduce prod res = P.toListM prod >>= (`shouldBe` res)

produceError, produceResult :: Producer (FilePath, AnalysisResult) IO ()
produceError  = each [("path/f.hs", Left "err!")]
produceResult = each [("f.hs", Right [ CC (ones, "g", 3)
                                     , CC (lo 2, "h", 5)
                                     , CC (lo 5, "f", 6)
                                     , CC (lo 7, "m", 10)
                                     , CC ((9, 2), "n", 15)
                                     ])]

-- | ANSI bold color
bold :: String
bold = setSGRCode [SetConsoleIntensity BoldIntensity]

-- | Make a ANSI foreground color sequence
fore :: Color -> String
fore color = setSGRCode [SetColor Foreground Dull color]

-- | ANSI sequence for reset
reset :: String
reset = setSGRCode []

spec :: Spec
spec = do
    describe "analyze" $ do
        it "accounts for case" $
            "case.hs" `shouldAnalyze` Right [CC (ones, "func", 3)]
        it "accounts for if..then..else" $
            "ifthenelse.hs" `shouldAnalyze` Right [CC (ones, "f", 2)]
        it "accounts for lambda case" $
            "lambdacase.hs" `shouldAnalyze` Right [CC (lo 2, "g", 3)]
        it "accounts for multi way if" $
            "multiif.hs" `shouldAnalyze` Right [CC (lo 2, "f", 4)]
        it "accounts for || operator" $
            "orop.hs" `shouldAnalyze` Right [CC (lo 1, "g", 3)]
        it "accounts for && operator" $
            "andop.hs" `shouldAnalyze` Right [CC (lo 1, "g", 3)]
        it "counts everything in a real example" $
            "stack-setup.hs" `shouldAnalyze`
                Right [ CC (lo 3, "ensureCompiler", 14)
                      , CC ((4, 9), "wc", 1)
                      , CC ((21, 9), "needLocal", 4)
                      , CC ((27, 9), "isWanted", 1)
                      , CC ((41, 17), "installedCompiler", 2)
                      , CC ((81, 37), "tool", 1)
                      , CC ((94, 17), "idents", 1)
                      , CC ((103, 21), "m", 1)
                      ]
        describe "extensions" $ do
-- Not even GHC 7.8.4 is able to run the file below, so it's not an Argon bug
#if __GLASGOW_HASKELL__ >= 710
            it "correctly applies CPP" $
                "cpp-psyn.hs" `shouldAnalyze` Right []
#endif
            it "applies CPP when needed" $
                "cpp.hs" `shouldAnalyze` Right [CC (lo 5, "f", 4)]
            it "works with TemplateHaskell" $
                "th.hs" `shouldAnalyze` Right [CC (lo 7, "foo", 1)]
            it "works with DataKinds, GADTs, KindSignatures" $
                "datakinds.hs" `shouldAnalyze`
                    Right [ CC (lo 16, "taskOneWorker", 1)
                          , CC (lo 20, "main", 1)]
            it "works with ScopedTypeVariables" $
                "scopedtypevariables.hs" `shouldAnalyze`
                    Right [CC (lo 9, "catchNonAsync", 1)]
            it "works with TypeFamilies" $
                "typefamilies.hs" `shouldAnalyze` Right []
            it "works with ForeignImport" $
                "foreignimports.hs" `shouldAnalyze` Right []
            it "works with Arrows" $
                "arrows.hs" `shouldAnalyze` Right [CC (lo 7, "getAnchor", 1)]
        describe "errors" $ do
            it "catches syntax errors" $
                "syntaxerror.hs" `shouldAnalyze`
                    Left ("2:1 parse error (possibly incorrect indentation" ++
                          " or mismatched brackets)")
            it "catches syntax errors (missing CPP)" $
                "missingcpp.hs" `shouldAnalyze`
                    Left "1:2 lexical error at character 'i'"
            it "catches syntax errors (missing cabal macros)" $
                unsafePerformIO (analyze defaultConfig (path "missingmacros.hs"))
                    `shouldSatisfy`
                    errStartsWith ("2:0  error: missing binary operator " ++
                                   "before token ")
            it "catches syntax errors (missing include dir)" $
                unsafePerformIO (analyze defaultConfig (path "missingincluded.hs"))
                    `shouldSatisfy`
                    errStartsWith ("2:0  fatal error: necessaryInclude.h: "
                                  ++ "No such file or directory")
            it "catches CPP parsing errors" $
                unsafePerformIO (analyze defaultConfig (path "cpp-error.hs"))
                    `shouldSatisfy`
                    errStartsWith "2:0  error: unterminated #else"
        describe "config" $ do
            it "reads default extensions from Cabal file" $
                ("missingcpp.hs", unsafePerformIO
                    (do loadedExts <- parseExts $ path "test.cabal"
                        return $ defaultConfig { exts = loadedExts }))
                    `shouldAnalyzeC`
                    Right [CC (lo 4, "f", 1)]
            it "reads other extensions from Cabal file" $
                ("missingcpp.hs", unsafePerformIO
                    (do loadedExts <- parseExts $ path "test-other.cabal"
                        return $ defaultConfig { exts = loadedExts }))
                    `shouldAnalyzeC`
                    Right [CC (lo 4, "f", 1)]
            it "reads old extensions from Cabal file" $
                ("missingcpp.hs", unsafePerformIO
                    (do loadedExts <- parseExts $ path "test-old.cabal"
                        return $ defaultConfig { exts = loadedExts }))
                    `shouldAnalyzeC`
                    Right [CC (lo 4, "f", 1)]
            it "includes Cabal macros for preprocessing" $
                ( "missingmacros.hs"
                , defaultConfig { headers = [path "cabal_macros.h"] }
                ) `shouldAnalyzeC` Right [CC (lo 3, "f", 2)]
            it "includes directory from include-dir for preprocessing" $
                ( "missingincluded.hs"
                , defaultConfig { includeDirs = [path "include"] }
                ) `shouldAnalyzeC` Right [CC (lo 5, "f", 3)]
    describe "Argon.Loc" $ do
        describe "srcSpanToLoc" $ do
            it "can convert a real src span to loc" $
                property $ \a b -> srcSpanToLoc (realSpan a b) == (a, b)
            it "can convert a bad src span to loc" $
                srcSpanToLoc GHC.noSrcSpan `shouldBe` (0, 0)
        describe "locToString" $
            it "can convert a loc to string" $
                locToString (1, 30) `shouldBe` "1:30"
        describe "tagMsg" $
            it "can tag messages" $
                tagMsg (2, 3) "my custom msg" `shouldBe` "2:3 my custom msg"
    describe "Argon.Results" $ do
        describe "order" $ do
            it "does not error on empty list" $
                order [] `shouldBe` []
            it "orders by complexity (descending)" $
                order [CC (ones, "f", 1), CC (lo 2, "f", 2)] `shouldBe`
                    [CC (lo 2, "f", 2), CC (ones, "f", 1)]
            it "orders by lines (ascending)" $
                order [CC (lo 11, "f", 3), CC (ones, "f", 3)] `shouldBe`
                    [CC (ones, "f", 3), CC (lo 11, "f", 3)]
            it "orders by function name (ascending)" $
                order [CC (lo 11, "g", 3), CC (lo 11, "f", 3)] `shouldBe`
                    [CC (lo 11, "f", 3), CC (lo 11, "g", 3)]
            it "does not add or remove elements" $
                property $ \xs -> sort xs == sort (order xs)
            it "is idempotent" $
                property $ \xs -> order xs == order (order xs)
        describe "filterNulls" $ do
            it "allows errors" $
                filterNulls ("", Left "err") `shouldBe` True
            it "disallows empty results" $
                filterNulls ("", Right []) `shouldBe` False
            it "always allows non-empty results" $
                property $ \x -> filterNulls ("", Right [x])
        describe "filterResults" $ do
            it "discards results with too low complexity" $
                filterResults (Config 3 [] [] [] BareText )
                              ("p", Right [ CC (ones, "f", 3)
                                          , CC (lo 2, "g", 2)
                                          , CC (lo 4, "h", 10)
                                          , CC (lo 3, "l", 1)])
                              `shouldBe`
                              ("p", Right [ CC (lo 4, "h", 10)
                                          , CC (ones, "f", 3)])
            it "does nothing on Left" $
                property $ \m o p err -> filterResults (Config m [] [] [] o)
                                                       (p, Left err) ==
                                                       (p, Left err)
    describe "Argon.Formatters" $ do
        describe "bareTextFormatter" $ do
            it "correctly formats errors" $
                (produceError >-> bareTextFormatter) `shouldProduce`
                    ["path/f.hs", "\terror: err!"]
            it "correctly formats results" $
                (produceResult >-> bareTextFormatter) `shouldProduce`
                    [ "f.hs"
                    , "\t1:1 g - 3"
                    , "\t2:1 h - 5"
                    , "\t5:1 f - 6"
                    , "\t7:1 m - 10"
                    , "\t9:2 n - 15"
                    ]
        describe "coloredTextFormatter" $ do
            it "correctly formats errors" $
                (produceError >-> coloredTextFormatter) `shouldProduce`
                    [ bold ++ "path/f.hs" ++ reset
                    , "\t" ++ fore Red ++ "error" ++ reset ++ ": err!"
                    ]
            it "correctly formats results" $
                (produceResult >-> coloredTextFormatter) `shouldProduce`
                    [ bold ++ "f.hs" ++ reset
                    , printf "\t1:1 %sg%s - %sA (3)%s" (fore Cyan) reset
                                                       (fore Green) reset
                    , printf "\t2:1 %sh%s - %sA (5)%s" (fore Cyan) reset
                                                       (fore Green) reset
                    , printf "\t5:1 %sf%s - %sB (6)%s" (fore Cyan) reset
                                                       (fore Yellow) reset
                    , printf "\t7:1 %sm%s - %sB (10)%s" (fore Cyan) reset
                                                        (fore Yellow) reset
                    , printf "\t9:2 %sn%s - %sC (15)%s" (fore Magenta) reset
                                                        (fore Red) reset
                    ]
    describe "Argon.Types" $ do
        describe "ComplexityBlock" $ do
            it "implements Show correctly" $
                show (CC ((2, 3), "bla bla", 32)) `shouldBe`
                    "CC ((2,3),\"bla bla\",32)"
            it "implements Eq correctly" $
                CC ((1, 4), "fun", 13) `shouldBe` CC ((1, 4), "fun", 13)
            it "implements Ord correctly" $
                CC (lo 1, "g", 2) < CC (lo 2, "f", 1) `shouldBe` True
        describe "OutputMode" $ do
            it "implements Show correctly" $
                show [JSON, Colored, BareText] `shouldBe`
                    "[JSON,Colored,BareText]"
            it "implements Eq correctly" $
                [JSON, Colored, BareText] `shouldBe` [JSON, Colored, BareText]
        describe "ToJSON instance" $ do
            it "is implemented by ComplexityResult" $
                encode (CC ((1, 3), "f", 4)) `shouldBe`
                    "{\"complexity\":4,\"name\":\"f\",\"lineno\":1,\"col\":3}"
            it "is implemented by (FilePath, AnalysisResult)" $
                encode ("f.hs" :: String, Right [] :: AnalysisResult)
                    `shouldBe`
                    "{\"blocks\":[],\"path\":\"f.hs\",\"type\":\"result\"}"
            it "is implemented by (FilePath, AnalysisResult) II" $
                encode ("f.hs" :: String, Left "err" :: AnalysisResult)
                    `shouldBe`
                    "{\"path\":\"f.hs\",\"type\":\"error\",\"message\":\"err\"}"
#if 0
    describe "Argon.Walker" $
        describe "allFiles" $ do
            it "traverses the filesystem depth-first" $
                allFiles ("test" </> "tree") `shouldProduceS`
                    [ "test" </> "tree" </> "sub"  </> "b.hs"
                    , "test" </> "tree" </> "sub"  </> "c.hs"
                    , "test" </> "tree" </> "sub2" </> "a.hs"
                    , "test" </> "tree" </> "sub2" </> "e.hs"
                    , "test" </> "tree" </> "a.hs"
                    ]
            it "includes starting files in the result" $
                allFiles ("test" </> "tree" </> "a.hs") `shouldProduceS`
                    ["test" </> "tree" </> "a.hs"]
#endif
