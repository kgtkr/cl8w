{-# LANGUAGE TemplateHaskell #-}


module Gen.OpCodeGen where

import qualified Data.DList                    as D
import qualified Wasm.AST                      as WA
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Parsers.Member                as PM
import qualified Parsers.Lang                  as PL
import qualified Parsers.Expr                  as PE
import           Control.Lens
import qualified Gen.Lang                      as WL
import           Control.Monad.Reader

type OpCodes=D.DList WA.OperatorCode
type Locals=D.DList WA.ValueType
type LocalsLen=Int
type LocalsMap=M.Map String LocalData
type LocalData=(PL.Type,Int)

data OpCodeGenData=OpCodeGenData{
    _opCodes::OpCodes,
    _locals:: Locals,
    _localsLen:: LocalsLen,
    _localsMap:: LocalsMap
}
makeLenses ''OpCodeGenData

emptyOpCodeGenData :: [(PL.Ident, PL.Type)] -> OpCodeGenData
emptyOpCodeGenData lo = OpCodeGenData
    { _opCodes   = D.empty
    , _locals    = D.empty
    , _localsLen = length lo
    , _localsMap = M.fromList
        $ (map (\(i, (name, t)) -> (name, (t, i))) . zip [0 ..]) lo
    }

type OpCodeGen = ReaderT WL.MemberData (State OpCodeGenData)
