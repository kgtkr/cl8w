module Parsers.Lang where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr

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

type Ident=String

data Type = TI32
          | TI64
          | TF32
          | TF64
          | TString
          | TArray Type
          | TBool
          | TChar
          | TStruct Ident
          deriving (Show, Eq)
