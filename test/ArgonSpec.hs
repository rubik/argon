{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module ArgonSpec (spec)
    where

import Data.List (sort, isPrefixOf)
import Data.Either (isLeft, lefts)
import qualified Data.Sequence as S
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Test.Hspec
import Test.QuickCheck
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import qualified SrcLoc     as GHC
import qualified FastString as GHC

import Argon
import Argon.Loc

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

path :: String -> FilePath
path f = "test" </> "data" </> f

shouldAnalyze :: String -> AnalysisResult -> Expectation
shouldAnalyze f r = analyze defaultConfig p `shouldReturn` (p, r)
    where p = path f

spec :: Spec
spec = do
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
            it "catches CPP parsing errors" $
                unsafePerformIO (analyze defaultConfig (path "cpp-error.hs"))
                `shouldSatisfy`
                \(_, res) ->
                    isLeft res && ("2:0  error: unterminated #else"
                                   `isPrefixOf` head (lefts [res]))
    describe "Argon.Walker" $
        describe "allFiles" $ do
            it "traverses the filesystem with reversed DFS" $
                allFiles ["test" </> "tree"] `shouldReturn`
                    S.fromList [ "test" </> "tree" </> "a.hs"
                               , "test" </> "tree" </> "sub2" </> "e.hs"
                               , "test" </> "tree" </> "sub2" </> "a.hs"
                               , "test" </> "tree" </> "sub"  </> "c.hs"
                               , "test" </> "tree" </> "sub"  </> "b.hs"
                               ]
            it "includes starting files in the result" $
                allFiles ["test" </> "tree" </> "a.hs"] `shouldReturn`
                    S.fromList ["test" </> "tree" </> "a.hs"]
