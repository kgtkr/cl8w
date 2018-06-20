module Parsers.Stat where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr
import qualified Parsers.Expr                  as E
import qualified Parsers.Lang                  as L

data SetIdent=SIIdent L.Ident
              |SIField SetIdent L.Ident
              |SIIndex SetIdent E.Expr
              deriving (Show, Eq)

setIdentP :: Parser SetIdent
setIdentP = try siIdentP <|> try siFieldP <|> try siIndexP

siIdentP :: Parser SetIdent
siIdentP = do
  ident <- L.identifier
  return $ SIIdent ident

siFieldP :: Parser SetIdent
siFieldP = do
  si <- setIdentP
  L.dot
  ident <- L.identifier
  return $ SIField si ident

siIndexP :: Parser SetIdent
siIndexP = do
  si <- setIdentP
  e  <- L.brackets E.exprP
  return $ SIIndex si e

data Stat =SBlock [Stat]
            |SExprToStat E.Expr
            |SLet L.Ident L.Type E.Expr
            |SIf (E.Expr,Stat) [(E.Expr,Stat)] (Maybe Stat)
            |SWhile E.Expr Stat
            |SReturn (Maybe E.Expr)
            |SSet SetIdent E.Expr
            deriving (Show, Eq)

statP :: Parser Stat
statP =
  try blockP
    <|> try exprToStatP
    <|> try letP
    <|> try ifP
    <|> try whileP
    <|> try returnP
    <|> try setP

exprToStatP :: Parser Stat
exprToStatP = do
  e <- E.exprP
  L.semi
  return $ SExprToStat e


blockP :: Parser Stat
blockP = L.braces
  (do
    ss <- many statP
    return $ SBlock ss
  )

letP :: Parser Stat
letP = do
  L.reserved "let"
  ident <- L.identifier
  L.colon
  t <- L.typeParser
  L.reservedOp "="
  e <- E.exprP
  L.semi
  return $ SLet ident t e

ifP :: Parser Stat
ifP = do
  L.reserved "if"
  e    <- E.exprP
  s    <- statP
  elif <- many
    (do
      L.reserved "else"
      L.reserved "if"
      ee <- E.exprP
      ss <- statP
      return (ee, ss)
    )
  el <- optionMaybe
    (do
      L.reserved "else"
      ss <- statP
      return ss
    )

  return $ SIf (e, s) elif el

whileP :: Parser Stat
whileP = do
  L.reserved "while"
  e <- E.exprP
  s <- statP
  return $ SWhile e s

returnP :: Parser Stat
returnP = do
  L.reserved "return"
  e <- optionMaybe E.exprP
  L.semi
  return $ SReturn e

setP :: Parser Stat
setP = do
  si <- setIdentP
  L.reservedOp "="
  e <- E.exprP
  L.semi
  return $ SSet si e
