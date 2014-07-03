{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Test.Hspec
import Text.Parser.Toml (Toml)

main :: IO ()
main = hspec $
    describe "corner cases" $ do
        it "single vs multi dict" $
            shouldBeInvalid "[[servers]]\n[servers]"

        it "nested single/multi" $
            shouldBeInvalid "[[a.b]]\n[a.b]"

        it "overwriting keys" $
            shouldBeInvalid "[foo]\nbar=1\n[foo.bar]\nbaz=2"

        it "redefining tables" $
            shouldBeInvalid "[foo]\n[foo]"

shouldBeInvalid :: Toml -> Expectation
shouldBeInvalid s = evaluate s `shouldThrow` anyException
