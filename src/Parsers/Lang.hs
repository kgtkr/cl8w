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

op =
  [ "!"
  , "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "&&"
  , "||"
  , "&"
  , "|"
  , "^"
  , "**"
  , "=="
  , "<"
  , "<="
  , ">"
  , ">="
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
  , reservedOpNames = op
  , reservedNames   = keywords
  , caseSensitive   = True
  }

tokenParser = P.makeTokenParser def

identifier :: Parser String
identifier = P.identifier tokenParser

operator :: Parser String
operator = P.operator tokenParser

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp tokenParser

reserved :: String -> Parser ()
reserved = P.reserved tokenParser

charLiteral :: Parser Char
charLiteral = P.charLiteral tokenParser

stringLiteral :: Parser String
stringLiteral = P.stringLiteral tokenParser

integer :: Parser Integer
integer = P.integer tokenParser

float :: Parser Double
float = P.float tokenParser

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace tokenParser

parens :: Parser a -> Parser a
parens = P.parens tokenParser

braces :: Parser a -> Parser a
braces = P.braces tokenParser

brackets :: Parser a -> Parser a
brackets = P.brackets tokenParser

semi :: Parser String
semi = P.semi tokenParser

comma :: Parser String
comma = P.comma tokenParser

colon :: Parser String
colon = P.colon tokenParser

dot :: Parser String
dot = P.dot tokenParser

semiSep :: Parser a -> Parser [a]
semiSep = P.semiSep tokenParser

semiSep1 :: Parser a -> Parser [a]
semiSep1 = P.semiSep1 tokenParser

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep tokenParser

commaSep1 :: Parser a -> Parser [a]
commaSep1 = P.commaSep1 tokenParser

type Ident=String

data Type = TI32
          | TI64
          | TF32
          | TF64
          | TString
          | TArray Type
          | TBool
          | TChar
          | TFunc [Type] (Maybe Type)
          | TStruct Ident
          deriving (Show, Eq)

typeParser :: Parser Type
typeParser =
  try
      (do
        reserved "i32"
        return TI32
      )
    <|> try
          (do
            reserved "i64"
            return TI64
          )
    <|> try
          (do
            reserved "f32"
            return TF32
          )
    <|> try
          (do
            reserved "f64"
            return TF64
          )
    <|> try
          (do
            reserved "string"
            return TString
          )
    <|> try
          (do
            t <- brackets typeParser
            return $ TArray t
          )
    <|> try
          (do
            reserved "bool"
            return TBool
          )
    <|> try
          (do
            reserved "char"
            return TChar
          )
    <|> try
          (do
            params <- (parens . semiSep) typeParser
            ret    <- optionMaybe
              (do
                reservedOp "->"
                typeParser
              )
            return $ TFunc params ret
          )
    <|> try (TStruct <$> identifier)
