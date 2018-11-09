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
        -- 前置演算子
        |ENot Expr
        |EPlus Expr
        |EMinus Expr
        -- 後置演算子
        | EMember L.Ident Expr
        | EIndex Expr Expr
        |ECall [Expr] Expr
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
        |EBlock [Expr] (Maybe Expr)
        |ELet L.Ident Expr
        |EIf (Expr,Expr) [(Expr,Expr)] (Maybe Expr)
        |EWhile Expr Expr
        |EReturn (Maybe Expr)
        |ESet Expr Expr
        |EFor Expr Expr Expr Expr
        |ELambda [L.Ident] [(L.Ident,L.Type)] L.Type Expr
      deriving (Show, Eq)

blockP :: Parser Expr
blockP =
  L.braces (EBlock <$> ((many . try) (exprP <* L.semi)) <*> optionMaybe exprP)

lambdaP :: Parser Expr
lambdaP = do
  L.reservedOp "\\"
  cap    <- (L.brackets . L.semiSep) L.identifier
  params <-
    (L.parens . L.semiSep) $ (,) <$> (L.identifier <* L.colon) <*> L.typeParser
  L.colon
  ret <- L.typeParser
  L.reservedOp "=>"
  e <- exprP
  return $ ELambda cap params ret e

letP :: Parser Expr
letP =
  ELet <$> (L.reserved "let" *> L.identifier) <*> (L.reservedOp "=" *> exprP)

ifP :: Parser Expr
ifP =
  EIf
    <$> ((,) <$> (L.reserved "if" *> L.parens exprP) <*> exprP)
    <*> many
          (   (,)
          <$> (try (L.reserved "else" >> L.reserved "if") *> L.parens exprP)
          <*> exprP
          )
    <*> optionMaybe (L.reserved "else" *> exprP)

whileP :: Parser Expr
whileP = EWhile <$> (L.reserved "while" *> L.parens exprP) <*> exprP

returnP :: Parser Expr
returnP = EReturn <$> (L.reserved "return" *> optionMaybe exprP)

forP :: Parser Expr
forP = do
  L.reserved "for"
  (a, b, c) <- L.parens $ do
    a <- exprP
    L.semi
    b <- exprP
    L.semi
    c <- exprP
    return (a, b, c)
  d <- exprP
  return $ EFor a b c d

exprP :: Parser Expr
exprP = buildExpressionParser table termP

termP =
  try f32LP
    <|> try f64LP
    <|> try i32LP
    <|> try i64LP
    <|> stringLP
    <|> charLP
    <|> arrayLP
    <|> boolLP
    <|> try structLP
    <|> varP
    <|> parensP
    <|> blockP
    <|> letP
    <|> ifP
    <|> whileP
    <|> returnP
    <|> forP
    <|> lambdaP

parensP :: Parser Expr
parensP = L.parens exprP

structLP :: Parser Expr
structLP = EStructL <$> L.identifier <*> (L.braces . L.commaSep)
  ((,) <$> L.identifier <* L.colon <*> exprP)

i32LP :: Parser Expr
i32LP =
  EI32L
    .   read
    <$> (  L.whiteSpace
        *> many1 digit
        <* (optional . string) "i32"
        <* L.whiteSpace
        )

i64LP :: Parser Expr
i64LP =
  EI64L . read <$> (L.whiteSpace *> many1 digit <* string "i64" <* L.whiteSpace)

f32LP :: Parser Expr
f32LP = do
  n <- L.whiteSpace *> many1 digit <* char '.'
  m <- many1 digit <* string "f32" <* L.whiteSpace
  return $ (EF32L . read) (n ++ "." ++ m)

f64LP :: Parser Expr
f64LP = do
  n <- L.whiteSpace *> many1 digit <* char '.'
  m <- many1 digit <* (optional . string) "f64" <* L.whiteSpace
  return $ (EF64L . read) (n ++ "." ++ m)

stringLP :: Parser Expr
stringLP = EStringL <$> L.stringLiteral

charLP :: Parser Expr
charLP = ECharL <$> L.charLiteral

arrayLP :: Parser Expr
arrayLP = L.brackets $ EArrayL <$> L.typeParser <* L.semi <*> exprP

boolLP :: Parser Expr
boolLP =
  EBoolL True <$ L.reserved "true" <|> EBoolL False <$ L.reserved "false"


varP :: Parser Expr
varP = EVar <$> L.identifier

-- https://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported?lq=1
prefix p = Prefix . chainl1 p $ return (.)
postfix p = Postfix . chainl1 p $ return (flip (.))

table =
  [ [ (postfix . choice)
        [ (EMember <$> (L.dot *> L.identifier))
        , EIndex <$> L.brackets exprP
        , ECall <$> (L.parens . L.commaSep) exprP
        ]
    ]
  , [ (prefix . choice)
        [ (ENot <$ L.reservedOp "!")
        , (EPlus <$ L.reservedOp "+")
        , (EMinus <$ L.reservedOp "-")
        ]
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
  , [Infix (L.reservedOp "=" >> return ESet) AssocLeft]
  ]
