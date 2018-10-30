module Gen.OpCodeGen where

import qualified Data.DList                    as D
import qualified Wasm.AST                      as WA
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Parsers.Member                as PM
import qualified Parsers.Lang                  as PL
import qualified Parsers.Expr                  as PE
import           Control.Lens
import qualified Gen.Lang                      as GL
import           Control.Monad.Reader

type OpCodes=D.DList WA.OperatorCode
type Locals=D.DList WA.ValueType
type LocalsLen=Int
type SymbolMap=M.Map String (PL.Type,SymbolData)
data SymbolData=SDLocal Int|SDFunc Int

type Params=[(PL.Ident, PL.Type)]

data OpCodeGenData=OpCodeGenData{
    _opCodeGenDataOpCodes::OpCodes,
    _opCodeGenDataLocals:: Locals,
    _opCodeGenDataLocalsLen:: LocalsLen,
    _opCodeGenDataSymbolMap:: SymbolMap,
    _opCodeGenDataParams::Params
}
makeFields ''OpCodeGenData

funcDefToType :: PM.FuncDef -> PL.Type
funcDefToType fd =
    PL.RefType (PL.TFunc ((map snd) (fd ^. PM.params)) (fd ^. PM.result))

emptyOpCodeGenData :: GL.FunctionMap -> Params -> OpCodeGenData
emptyOpCodeGenData fm ps = OpCodeGenData
    { _opCodeGenDataOpCodes   = D.empty
    , _opCodeGenDataLocals    = D.empty
    , _opCodeGenDataLocalsLen = length ps
    , _opCodeGenDataSymbolMap = fmMap
    , _opCodeGenDataParams    = ps
    }
    where fmMap = fmap (\(a, b) -> (funcDefToType b, SDFunc a)) fm

type OpCodeGen = ReaderT GL.MemberData (State OpCodeGenData)
