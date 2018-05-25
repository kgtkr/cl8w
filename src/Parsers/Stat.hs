module Parsers.Stat where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import           Text.ParserCombinators.Parsec.Expr
import           Parsers.Expr
import           Parsers.Lang

data SetIdent=SIIdent Ident
              |SIField SetIdent Ident
              |SIIndex SetIdent Expr
              deriving (Show, Eq)

data Stat =SBlock [Stat]
            |SExprToStat Expr
            |SLet Ident Type Expr
            |SIf Expr Stat
            |SWhile Expr Stat
            |SReturn (Maybe Expr)
            |SSet SetIdent Expr
            deriving (Show, Eq)
