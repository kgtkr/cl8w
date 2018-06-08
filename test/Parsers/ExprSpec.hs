module Parsers.ExprSpec where

import           Text.ParserCombinators.Parsec
import           Test.Hspec
import           Parsers.Expr
import           Parsers.Lang

spec :: Spec
spec = do
  describe "exprP" $ do
    it "test" $ do
      (parse exprP "test" "T{x:1}")
        `shouldBe` Right (EStructL "T" [("x", EI32L 1)])
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

      (parse exprP "test" "!x") `shouldBe` Right ((ENot . EVar) "x")
      (parse exprP "test" "+x") `shouldBe` Right ((EPlus . EVar) "x")
      (parse exprP "test" "-x") `shouldBe` Right ((EMinus . EVar) "x")

      (parse exprP "test" "x.a") `shouldBe` Right (EMember "a" (EVar "x"))
      (parse exprP "test" "x[i]")
        `shouldBe` Right (EIndex (EVar "i") (EVar "x"))
      (parse exprP "test" "x(1,2)")
        `shouldBe` Right (ECall [EI32L 1, EI32L 2] (EVar "x"))

      (parse exprP "test" "x+y") `shouldBe` Right (EAdd (EVar "x") (EVar "y"))

