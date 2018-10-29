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
type SymbolMap=M.Map String (PL.Type,SymbolData)
data SymbolData=SDLocal Int|SDConst Int

data OpCodeGenData=OpCodeGenData{
    _opCodeGenDataOpCodes::OpCodes,
    _opCodeGenDataLocals:: Locals,
    _opCodeGenDataLocalsLen:: LocalsLen,
    _opCodeGenDataSymbolMap:: SymbolMap
}
makeFields ''OpCodeGenData

emptyOpCodeGenData :: [(PL.Ident, PL.Type)] -> OpCodeGenData
emptyOpCodeGenData lo = OpCodeGenData
    { _opCodeGenDataOpCodes   = D.empty
    , _opCodeGenDataLocals    = D.empty
    , _opCodeGenDataLocalsLen = length lo
    , _opCodeGenDataSymbolMap = M.fromList
        $ (map (\(i, (name, t)) -> (name, (t, SDLocal i))) . zip [0 ..]) lo
    }

type OpCodeGen = ReaderT WL.MemberData (State OpCodeGenData)
