module WasmGen.Expr where

import           Data.Monoid
import           Data.Semigroup
import qualified Data.DList                    as D
import qualified Wasm.AST                      as W
import qualified Data.Map                      as M

-- コード,ローカル変数リスト,ローカル変数の長さ,ローカル変数名
data ExprGenData=ExprGenData (D.DList W.OperatorCode) (D.DList W.ValueType) Int (M.Map String Int)

instance Monoid ExprGenData where
    mempty = ExprGenData mempty mempty 0 mempty
    ExprGenData a1 a2 a3 a4 `mappend` ExprGenData b1 b2 b3 b4=ExprGenData (mappend a1 b1) (mappend a2 b2) (a3 + b3) (mempty a4 b4)

