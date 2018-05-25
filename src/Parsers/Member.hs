module Parsers.Member where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr
import           Parsers.Stat
import           Parsers.Lang

data FuncDef=FuncDef{
  name::Ident,
  params::[(Ident,Type)],
  result::(Maybe Type)
}deriving (Show, Eq)

data Member=MStruct Ident [(Ident,Type)]
            |MFun FuncDef Stat
            |MExternFun FuncDef String
            deriving (Show, Eq)
