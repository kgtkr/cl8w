module Parsers.Expr where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr
import qualified Parsers.Lang                  as L

data Expr = EStructL L.Ident [(L.Ident,Expr)]
        |EI32L Int
        |EI64L Int
        |EF32L Float
        |EF64L Double
        |EStringL String
        |EArrayL L.Type Expr
        |EBoolL Bool
        |ECharL Char
        |EVar L.Ident
        |ECall L.Ident [Expr]
        -- 前置演算子
        |ENot Expr
        |EPlus Expr
        |EMinus Expr
        -- 後置演算子
        | EMember L.Ident Expr
        | EIndex Expr Expr
        -- 二項演算子
        |EAdd Expr Expr
        |ESub Expr Expr
        |EMul Expr Expr
        |EDiv Expr Expr
        |EMod Expr Expr
        |EAnd Expr Expr
        |EOr Expr Expr
        |EBitAnd Expr Expr
        |EBitOr Expr Expr
        |EBitXor Expr Expr
        |EPow Expr Expr
        |EEq Expr Expr
        |ENe Expr Expr
        |ELt Expr Expr
        |ELte Expr Expr
        |EGt Expr Expr
        |EGte Expr Expr
      deriving (Show, Eq)

exprP :: Parser Expr
exprP = buildExpressionParser table termP

termP =
  try structLP
    <|> try f32LP
    <|> try f64LP
    <|> try i32LP
    <|> try i64LP
    <|> try stringLP
    <|> try charLP
    <|> try arrayLP
    <|> try boolLP
    <|> try varP
    <|> try parensP
    <|> try callP

parensP :: Parser Expr
parensP = L.parens exprP

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

stringLP :: Parser Expr
stringLP = EStringL <$> L.stringLiteral

charLP :: Parser Expr
charLP = ECharL <$> L.charLiteral

arrayLP :: Parser Expr
arrayLP = L.brackets
  (do
    t <- L.typeParser
    L.semi
    e <- exprP
    return $ EArrayL t e
  )

boolLP :: Parser Expr
boolLP = (<|>)
  (do
    L.reserved "true"
    return $ EBoolL True
  )
  (do
    L.reserved "false"
    return $ EBoolL False
  )

varP :: Parser Expr
varP = EVar <$> L.identifier

callP :: Parser Expr
callP = do
  ident <- L.identifier
  es    <- (L.parens . L.commaSep) exprP
  return $ ECall ident es
table =
  [ [ Postfix
      (do
        L.dot
        EMember <$> L.identifier
      )
    , Postfix $ EIndex <$> L.brackets exprP
    ]
  , [ Prefix
      (do
        L.reservedOp "!"
        return ENot
      )
    , Prefix
      (do
        L.reservedOp "+"
        return EPlus
      )
    , Prefix
      (do
        L.reservedOp "-"
        return EMinus
      )
    ]
  , [Infix (L.reservedOp "**" >> return EPow) AssocLeft]
  , [ Infix (L.reservedOp "*" >> return EMul) AssocLeft
    , Infix (L.reservedOp "/" >> return EDiv) AssocLeft
    , Infix (L.reservedOp "%" >> return EMod) AssocLeft
    ]
  , [ Infix (L.reservedOp "+" >> return EAdd) AssocLeft
    , Infix (L.reservedOp "-" >> return ESub) AssocLeft
    ]
  , [ Infix (L.reservedOp "<" >> return ELt)   AssocLeft
    , Infix (L.reservedOp "<=" >> return ELte) AssocLeft
    , Infix (L.reservedOp ">" >> return EGt)   AssocLeft
    , Infix (L.reservedOp ">=" >> return EGte) AssocLeft
    ]
  , [ Infix (L.reservedOp "==" >> return EEq) AssocLeft
    , Infix (L.reservedOp "!=" >> return ENe) AssocLeft
    ]
  , [ Infix (L.reservedOp "&" >> return EBitAnd) AssocLeft
    , Infix (L.reservedOp "|" >> return EBitOr)  AssocLeft
    , Infix (L.reservedOp "^" >> return EBitXor) AssocLeft
    ]
  , [ Infix (L.reservedOp "&&" >> return EAnd) AssocLeft
    , Infix (L.reservedOp "||" >> return EOr)  AssocLeft
    ]
  ]
