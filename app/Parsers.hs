module Parsers where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr

type Ident=String

keywords =
  [ "i32"
  , "i64"
  , "f32"
  , "f64"
  , "string"
  , "bool"
  , "char"
  , "true"
  , "false"
  , "null"
  , "let"
  , "if"
  , "while"
  , "return"
  , "struct"
  , "fun"
  , "extern"
  ]

def :: LanguageDef st
def = LanguageDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , identStart      = letter
  , identLetter     = alphaNum <|> oneOf "_"
  , opStart         = oneOf "+-*/%&|^=<>!"
  , opLetter        = oneOf "+-*/%&|^=<>!"
  , reservedOpNames = []
  , reservedNames   = keywords
  , caseSensitive   = True
  }

tokenParser = P.makeTokenParser def

data Type = TI32
          | TI64
          | TF32
          | TF64
          | TString
          | TArray Type
          | TBool
          | TChar
          | TStruct Ident
          deriving Show

data Expr = ECall Ident [Expr]
        |EStructL Ident [(Ident,Expr)]
        |EI32L Int
        |EI64L Integer
        |EF32L Float
        |EF64L Double
        |EStringL String
        |EArrayL Type [Expr]
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
      deriving Show

identP :: Parser String
identP = P.identifier tokenParser

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

data SetIdent=SIIdent Ident
              |SIField SetIdent Ident
              |SIIndex SetIdent Expr
              deriving Show

data Stat =SBlock [Stat]
            |SExprToStat Expr
            |SLet Ident Type Expr
            |SIf Expr Stat
            |SWhile Expr Stat
            |SReturn (Maybe Expr)
            |SSet SetIdent Expr
            deriving Show

data FuncDef=FuncDef{
  name::Ident,
  params::[(Ident,Type)],
  result::(Maybe Type)
}deriving Show

data Member=MStruct Ident [(Ident,Type)]
            |MFun FuncDef Stat
            |MExternFun FuncDef String
            deriving Show
