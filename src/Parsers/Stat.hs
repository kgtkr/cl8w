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
setIdentP = siIdentP <|> siFieldP <|> siIndexP

siIdentP :: Parser SetIdent
siIdentP = SIIdent <$> L.identifier

siFieldP :: Parser SetIdent
siFieldP = SIField <$> setIdentP <*> (L.dot *> L.identifier)

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
statP = blockP <|> letP <|> ifP <|> whileP <|> returnP <|> exprToStatP -- <|> setP

exprToStatP :: Parser Stat
exprToStatP = SExprToStat <$> E.exprP <* L.semi


blockP :: Parser Stat
blockP = L.braces (SBlock <$> many statP)

letP :: Parser Stat
letP =
  SLet
    <$> (L.reserved "let" *> L.identifier)
    <*> (L.colon *> L.typeParser)
    <*> (L.reservedOp "=" *> E.exprP <* L.semi)

ifP :: Parser Stat
ifP =
  SIf
    <$> ((,) <$> (L.reserved "if" *> L.parens E.exprP) <*> statP)
    <*> many
          (   (,)
          <$> (try (L.reserved "else" >> L.reserved "if") *> L.parens E.exprP)
          <*> statP
          )
    <*> optionMaybe (L.reserved "else" *> statP)

whileP :: Parser Stat
whileP = SWhile <$> (L.reserved "while" *> L.parens E.exprP) <*> statP

returnP :: Parser Stat
returnP = SReturn <$> (L.reserved "return" *> optionMaybe E.exprP <* L.semi)

setP :: Parser Stat
setP = SSet <$> setIdentP <*> (L.reservedOp "=" *> E.exprP <* L.semi)
