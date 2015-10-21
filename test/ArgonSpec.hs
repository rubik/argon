{-# LANGUAGE CPP #-}
module ArgonSpec (spec)
    where

import Data.List (sort)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Test.Hspec
import Test.QuickCheck
import Argon

instance Arbitrary ComplexityBlock where
    arbitrary = (\a b c d -> CC (a, b, c, d)) <$> arbitrary
                                              <*> arbitrary
                                              <*> arbitrary
                                              <*> arbitrary
    shrink (CC t) = map CC $ shrink t


shouldParse :: String -> AnalysisResult -> Expectation
shouldParse s r = parseCode (Just "fname") s `shouldReturn` ("fname", r)

spec :: Spec
spec = do
    describe "order" $ do
        it "does not error on empty list" $
            order [] `shouldBe` []
        it "orders by complexity (descending)" $
            order [CC (1, 1, "f", 1), CC (2, 1, "f", 2)] `shouldBe`
                [CC (2, 1, "f", 2), CC (1, 1, "f", 1)]
        it "orders by lines (ascending)" $
            order [CC (11, 1, "f", 3), CC (1, 1, "f", 3)] `shouldBe`
                [CC (1, 1, "f", 3), CC (11, 1, "f", 3)]
        it "orders by function name (ascending)" $
            order [CC (11, 1, "g", 3), CC (11, 1, "f", 3)] `shouldBe`
                [CC (11, 1, "f", 3), CC (11, 1, "g", 3)]
        it "does not remove or add elements" $
            property $ \xs -> sort xs == sort (order xs)
    describe "parseCode" $ do
        it "accounts for case" $
            unlines [ "f n = case n of"
                    , "        2 -> 24"
                    , "        3 -> 27"
                    , "        _ -> 49"] `shouldParse` Right [CC (1, 1, "f", 3)]
        it "accounts for if..then..else" $
            "f n = if n == 4 then 24 else 20" `shouldParse`
                Right [CC (1, 1, "f", 2)]
