module Parsers.Member where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr
import qualified Parsers.Stat                  as S
import qualified Parsers.Lang                  as L

data FuncDef=FuncDef{
  name::L.Ident,
  params::[(L.Ident,L.Type)],
  result::Maybe L.Type
}deriving (Show, Eq)

funcDefP :: Parser FuncDef
funcDefP = do
  name   <- L.identifier
  params <- L.parens
    (L.commaSep
      (do
        i <- L.identifier
        L.colon
        t <- L.typeParser
        return (i, t)
      )
    )
  result <- optionMaybe
    (do
      L.colon
      t <- L.typeParser
      return t
    )
  return $ FuncDef {name = name, params = params, result = result}

data Member=MStruct L.Ident [(L.Ident,L.Type)]
            |MFun FuncDef S.Stat
            |MExternFun FuncDef String
            deriving (Show, Eq)

memberP :: Parser Member
memberP = undefined

structP :: Parser Member
structP = do
  L.reserved "struct"
  ident <- L.identifier
  m     <- L.braces
    (L.commaSep
      (do
        i <- L.identifier
        L.colon
        t <- L.typeParser
        return (i, t)
      )
    )
  return $ MStruct ident m

funP :: Parser Member
funP = do
  L.reserved "fun"
  d <- funcDefP
  L.reservedOp "="
  s <- S.statP
  return $ MFun d s

externFunP :: Parser Member
externFunP = do
  L.reserved "extern"
  L.reserved "fun"
  s <- L.stringLiteral
  d <- funcDefP
  return $ MExternFun d s
