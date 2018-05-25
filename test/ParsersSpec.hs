module ParsersSpec where

import           Test.Hspec
import           Parsers

spec :: Spec
spec = do
  describe "exprP" $ do
    it "aaaa" $ 5 + 6 `shouldBe` 11
