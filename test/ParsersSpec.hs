module ParsersSpec where

import           Text.ParserCombinators.Parsec
import           Test.Hspec
import           Parsers.Expr

spec :: Spec
spec = do
  describe "exprP" $ do
    it "test" $ do
      (parse exprP "test" "Struct {x:Foo{},y:f(11i64 ,100,10i32)}") `shouldBe` Right
        (EStructL "Struct" [("x", EStructL "Foo" []), ("y", ECall "f" [EI64L 11,EI32L 100,EI32L 10])])
