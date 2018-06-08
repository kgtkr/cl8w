module Parsers.ExprSpec where

import           Text.ParserCombinators.Parsec
import           Test.Hspec
import           Parsers.Expr

spec :: Spec
spec = do
  describe "exprP" $ do
    it "test" $ do
      (parse exprP "test" "1") `shouldBe` Right (EI32L 1)
      (parse exprP "test" "1i32") `shouldBe` Right (EI32L 1)
      (parse exprP "test" "1i64") `shouldBe` Right (EI64L 1)
      (parse exprP "test" "\"aiueo\\n\"") `shouldBe` Right (EStringL "aiueo\n")

