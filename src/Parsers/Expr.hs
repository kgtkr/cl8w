module Parsers.Expr where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr
import qualified Parsers.Lang                  as L
import           Data.Int

data Expr = ECall L.Ident [Expr]
        |EStructL L.Ident [(L.Ident,Expr)]
        |EI32L Int32
        |EI64L Int64
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
        |EVar L.Ident
      deriving (Show, Eq)

exprP :: Parser Expr
exprP = try callP <|> try structLP <|> try i64LP <|> try i32LP

callP :: Parser Expr
callP = do
  ident <- L.identifier
  exprs <- (L.parens . L.commaSep) exprP
  return $ ECall ident exprs

structLP :: Parser Expr
structLP = do
  ident  <- L.identifier
  member <- (L.braces . L.commaSep)
    (do
      mIdent <- L.identifier
      L.colon
      mExpr <- exprP
      return (mIdent, mExpr)
    )
  return $ EStructL ident member

i32LP :: Parser Expr
i32LP = do
  L.whiteSpace
  x <- many1 digit
  (optional . string) "i32"
  L.whiteSpace
  return $ (EI32L . read) x

i64LP :: Parser Expr
i64LP = do
  L.whiteSpace
  x <- many1 digit
  string "i64"
  L.whiteSpace
  return $ (EI64L . read) x

f32LP :: Parser Expr
f32LP = do
  L.whiteSpace
  n <- many1 digit
  char '.'
  m <- many1 digit
  string "f32"
  L.whiteSpace
  return $ (EF32L . read) (n ++ "." ++ m)

f64LP :: Parser Expr
f64LP = do
  L.whiteSpace
  n <- many1 digit
  char '.'
  m <- many1 digit
  (optional . string) "f64"
  L.whiteSpace
  return $ (EF64L . read) (n ++ "." ++ m)
