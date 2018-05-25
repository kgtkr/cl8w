module ParsersSpec where

import           Text.ParserCombinators.Parsec
import           Test.Hspec
import           Parsers

spec :: Spec
spec = do
  describe "exprP" $ do
    it "test" $ do
      (parse exprP "test" "Struct {x:Foo{},y:f(),}") `shouldBe` Right
        (EStructL "Struct" [("x", EStructL "Foo" []), ("y", ECall "f" [])])
