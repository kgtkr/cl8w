module Parsers.Member where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr
import qualified Parsers.Stat                  as S
import qualified Parsers.Lang                  as L
import           Control.Lens

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
            |MFun FuncDef S.Stat
            |MExternFun FuncDef String
            deriving (Show, Eq)

memberP :: Parser Member
memberP = structP <|> funP <|> externFunP

membersP :: Parser [Member]
membersP = many memberP

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
  MFun <$> (L.reserved "fun" *> funcDefP) <*> (L.reservedOp "=" *> S.statP)

externFunP :: Parser Member
externFunP =
  flip MExternFun
    <$> (L.reserved "extern" *> L.reserved "fun" *> L.stringLiteral)
    <*> funcDefP
