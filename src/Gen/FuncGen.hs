module Gen.FuncGen where

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

data FuncGenData=FuncGenData{
    _funcGenDataOpCodes::OpCodes,
    _funcGenDataLocals:: Locals,
    _funcGenDataLocalsLen:: LocalsLen,
    _funcGenDataSymbolMap:: SymbolMap,
    _funcGenDataParams::Params,
    _funcGenDataMd :: GL.MemberData
}
makeFields ''FuncGenData

funcDefToType :: PM.FuncDef -> PL.Type
funcDefToType fd =
    PL.RefType (PL.TFunc ((map snd) (fd ^. PM.params)) (fd ^. PM.result))

emptyFuncGenData :: GL.MemberData -> GL.FunctionMap -> Params -> FuncGenData
emptyFuncGenData md fm ps = FuncGenData
    { _funcGenDataOpCodes   = D.empty
    , _funcGenDataLocals    = D.empty
    , _funcGenDataLocalsLen = length ps
    , _funcGenDataSymbolMap = fmMap
    , _funcGenDataParams    = ps
    , _funcGenDataMd        = md
    }
    where fmMap = fmap (\(a, b) -> (funcDefToType b, SDFunc a)) fm

type FuncGen = State FuncGenData
