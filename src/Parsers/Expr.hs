module Parsers.Expr where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr
import qualified Parsers.Lang                  as L

data Expr = ECall L.Ident [Expr]
        |EStructL L.Ident [(L.Ident,Expr)]
        |EI32L Int
        |EI64L Integer
        |EF32L Float
        |EF64L Double
        |EStringL String
        |EArrayL L.Type [Expr]
        |EBoolL Bool
        |ECharL Char
        |ENullE
        |EAdd Expr Expr
        |ESub Expr Expr
        |EMult Expr Expr
        |EDiv Expr Expr
        |EMod Expr Expr
        |EAnd Expr Expr
        |EOr Expr Expr
        |EBitAnd Expr Expr
        |EBitOr Expr Expr
        |EBitXor Expr Expr
        |EPow Expr Expr
        |EEq Expr Expr
        |ELt Expr Expr
        |ELte Expr Expr
        |EGt Expr Expr
        |EGte Expr Expr
        |ENot Expr
        |EIndex Expr Expr
        |EPlus Expr
        |EMinus Expr
      deriving (Show, Eq)

identP :: Parser String
identP = P.identifier L.tokenParser

exprP :: Parser Expr
exprP = try callP <|> structLP

callP :: Parser Expr
callP = do
  ident <- identP
  char '('
  exprs <- many
    (do
      expr <- exprP
      char ','
      return expr
    )

  char ')'
  return $ ECall ident exprs

structLP :: Parser Expr
structLP = do
  ident <- identP
  char '{'
  member <- many
    (do
      mIdent <- identP
      char ':'
      mExpr <- exprP
      char ','
      return (mIdent, mExpr)
    )
  char '}'
  return $ EStructL ident member
