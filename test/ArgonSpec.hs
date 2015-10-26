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
    describe "parseCode" $ do
        it "accounts for case" $
            "case.hs" `shouldAnalyze` Right [CC ((1, 1), "func", 3)]
        it "accounts for if..then..else" $
            "ifthenelse.hs" `shouldAnalyze` Right [CC ((1, 1), "f", 2)]
