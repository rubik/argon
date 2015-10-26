{-# LANGUAGE CPP #-}
module ArgonSpec (spec)
    where

import Data.List (sort)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Test.Hspec
import Test.QuickCheck
import System.FilePath ((</>))
import Argon

instance Arbitrary ComplexityBlock where
    arbitrary = (\a b c -> CC (a, b, c)) <$> arbitrary
                                         <*> arbitrary
                                         <*> arbitrary
    shrink (CC t) = map CC $ shrink t


ones :: Loc
ones = (1, 1)

lo :: Int -> Loc
lo s = (s, 1)

shouldAnalyze :: String -> AnalysisResult -> Expectation
shouldAnalyze f r = analyze path `shouldReturn` (path, r)
    where path = "test" </> "data" </> f

spec :: Spec
spec = do
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
        it "applies CPP when needed" $
            "cpp.hs" `shouldAnalyze` Right [CC (lo 5, "f", 4)]
        it "catches syntax errors" $
            "syntaxerror.hs" `shouldAnalyze`
                Left "2:1 parse error (possibly incorrect indentation or mismatched brackets)"
        it "catches CPP parsing errors" $
            "cpp-error.hs" `shouldAnalyze`
                Left "2:0  error: unterminated #else\n\t\t #if 0\n\t\t ^"
