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

data Stat =SBlock [Stat]
            |SExprToStat E.Expr
            |SLet L.Ident L.Type E.Expr
            |SIf E.Expr Stat
            |SWhile E.Expr Stat
            |SReturn (Maybe E.Expr)
            |SSet SetIdent E.Expr
            deriving (Show, Eq)
