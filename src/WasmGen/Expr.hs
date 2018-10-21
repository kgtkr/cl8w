module WasmGen.Expr where

import           Data.Monoid
import           Data.Semigroup
import qualified Data.DList                    as D
import qualified Wasm.AST                      as W

data ExprGenData=ExprGenData (D.DList W.OperatorCode) (D.DList W.ValueType)

instance Monoid ExprGenData where
    mempty = ExprGenData mempty mempty
    ExprGenData a1 a2 `mappend` ExprGenData b1 b2=ExprGenData (mappend a1 b1) (mappend a2 b2)

