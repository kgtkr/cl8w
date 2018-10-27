module Parsers.Member where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr
import qualified Parsers.Lang                  as L
import           Control.Lens
import qualified Parsers.Expr                  as E

data FuncDef = FuncDef {
  _funcDefName::L.Ident ,
  _funcDefParams::[(L.Ident,L.Type)],
  _funcDefResult:: Maybe L.Type
} deriving (Show, Eq)

makeFields ''FuncDef

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
      L.typeParser
    )
  return $ FuncDef name params result


type StructMember=(L.Ident,L.Type)
type StructMembers=[StructMember]

data Member=MStruct L.Ident StructMembers
            |MFun FuncDef E.Expr
            |MExternFun FuncDef String
            deriving (Show, Eq)

type Module=[Member]

memberP :: Parser Member
memberP = structP <|> funP <|> externFunP

moduleP :: Parser Module
moduleP = many memberP

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
funP =
  MFun <$> (L.reserved "fun" *> funcDefP) <*> (L.reservedOp "=" *> E.exprP)

externFunP :: Parser Member
externFunP =
  flip MExternFun
    <$> (L.reserved "extern" *> L.reserved "fun" *> L.stringLiteral)
    <*> funcDefP
