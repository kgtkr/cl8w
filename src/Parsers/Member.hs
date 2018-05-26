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

data Member=MStruct L.Ident [(L.Ident,L.Type)]
            |MFun FuncDef S.Stat
            |MExternFun FuncDef String
            deriving (Show, Eq)
