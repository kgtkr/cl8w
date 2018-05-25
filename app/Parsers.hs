module Parsers where

type Ident=String

data Type = TI32
          | TI64
          | TF32
          | TF64
          | TString
          | TArray Type
          | TBool
          | TChar
          | TStruct Ident

data Expr = EParens Expr
        |ECall Ident [Expr]
        |EStructL Ident [(Ident,Expr)]
        |EI32L Int
        |EI64L Integer
        |EF32L Float
        |EF64L Double
        |EStringL String
        |EArrayL Type [Expr]
        |EBoolL Bool
        |ECharL Char
        |ENullE
        |EAdd Expr Expr
        |ESub Expr Expr
        |EMult Expr Expr
        |EDiv Expr Expr
        |EMod Expr Expr
        |EAnd Expr Expr
        |EOr Expr Expr
        |EBitAnd Expr Expr
        |EBitOr Expr Expr
        |EBitXor Expr Expr
        |EPow Expr Expr
        |EEq Expr Expr
        |ELt Expr Expr
        |ELte Expr Expr
        |EGt Expr Expr
        |EGte Expr Expr
        |ENot Expr
        |EIndex Expr Expr
        |EPlus Expr
        |EMinus Expr

data SetIdent=SIIdent Ident
              |SIField SetIdent Ident
              |SIIndex SetIdent Expr

data Stat =SBlock [Stat]
            |SExprToStat Expr
            |SLet Ident Type Expr
            |SIf Expr Stat
            |SWhile Expr Stat
            |SReturn (Maybe Expr)
            |SSet SetIdent Expr

data FuncDef=FuncDef{
  name::Ident,
  params::[(Ident,Type)],
  result::(Maybe Type)
}

data Member=MStruct Ident [(Ident,Type)]
            |MFun FuncDef Stat
            |MExternFun FuncDef String

