module Parsers.ExprSpec where

import           Text.ParserCombinators.Parsec
import           Test.Hspec
import           Parsers.Expr

spec :: Spec
spec = do
  describe "exprP" $ do
    it "test" $ do
      (parse exprP "test" "Struct {x:Foo{}}")
        `shouldBe` Right (EStructL "Struct" [("x", EStructL "Foo" [])])

