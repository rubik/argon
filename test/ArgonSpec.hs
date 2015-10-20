module ArgonSpec (spec)
    where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck
import Argon


shouldParse :: String -> AnalysisResult -> Expectation
shouldParse s r = parseCode (Just "fname") s `shouldReturn` ("fname", r)

spec :: Spec
spec = do
    describe "order" $ do
        it "does not error on empty list" $
            order [] `shouldBe` []
        it "orders by complexity (descending)" $
            order [(1, 1, "f", 1), (2, 1, "f", 2)] `shouldBe`
                [(2, 1, "f", 2), (1, 1, "f", 1)]
        it "orders by lines (ascending)" $
            order [(11, 1, "f", 3), (1, 1, "f", 3)] `shouldBe`
                [(1, 1, "f", 3), (11, 1, "f", 3)]
        it "orders by function name (ascending)" $
            order [(11, 1, "g", 3), (11, 1, "f", 3)] `shouldBe`
                [(11, 1, "f", 3), (11, 1, "g", 3)]
        it "does not remove or add elements" $
            property $ \xs -> sort xs == sort (order xs)
    describe "parseCode" $ do
        it "accounts for case" $
            unlines [ "f n = case n of"
                    , "        2 -> 24"
                    , "        3 -> 27"
                    , "        _ -> 49"] `shouldParse` Right [(1, 1, "f", 3)]
        it "accounts for if..then..else" $
            "f n = if n == 4 then 24 else 20" `shouldParse`
                Right [(1, 1, "f", 2)]
