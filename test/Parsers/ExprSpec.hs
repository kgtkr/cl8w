module Parsers.ExprSpec where

import           Text.ParserCombinators.Parsec
import           Test.Hspec
import           Parsers.Expr
import           Parsers.Lang

spec :: Spec
spec = do
  describe "exprP" $ do
    it "test" $ do
      (parse exprP "test" "1") `shouldBe` Right (EI32L 1)
      (parse exprP "test" "1i32") `shouldBe` Right (EI32L 1)
      (parse exprP "test" "1i64") `shouldBe` Right (EI64L 1)
      (parse exprP "test" "\"aiueo\\n\"") `shouldBe` Right (EStringL "aiueo\n")
      (parse exprP "test" "[i32;1]") `shouldBe` Right (EArrayL TI32 (EI32L 1))
      (parse exprP "test" "true") `shouldBe` Right (EBoolL True)
      (parse exprP "test" "false") `shouldBe` Right (EBoolL False)
      (parse exprP "test" "'a'") `shouldBe` Right (ECharL 'a')
      (parse exprP "test" "null") `shouldBe` Right (ENullL)
      (parse exprP "test" "x") `shouldBe` Right (EVar "x")

