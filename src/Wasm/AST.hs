module Wasm.AST where
import qualified Data.Binary                   as B
import qualified Data.ByteString               as BS
import           Data.Int
import           Data.Binary.Get

data ValueType = I32|I64|F32|F64

data Type = ValueType ValueType|AnyFunc|Func|BlockType (Maybe ValueType)

type BlockType = Maybe ValueType
data FuncCmd =
    Unreachable
    |Nop
    |Block BlockType
    |Loop BlockType
    |If BlockType
    |Else
    |End
    |Br Int
    |BrIf Int
    |BrTable Int [Int] Int
    |Return
    |Call Int
    |CallIndirect Int Int
    |Drop
    |Select
    |GetLocal Int
    |SetLocal Int
    |TeeLocal Int
    |GetGlobal Int
    |SetGlobal Int
    |I32Load Int Int
    |I64Load Int Int
    |F32Load Int Int
    |F64Load Int Int
    |I32Load8s Int Int
    |I32Load8u Int Int
    |I32Load16s Int Int
    |I32Load16u Int Int
    |I64Load8s Int Int
    |I64Load8u Int Int
    |I64Load16s Int Int
    |I64Load16u Int Int
    |I64Load32s Int Int
    |I64Load32u Int Int
    |I32Store Int Int
    |I64Store Int Int
    |F32Store Int Int
    |F64Store Int Int
    |I32Store8 Int Int
    |I32Store16 Int Int
    |I64Store8 Int Int
    |I64Store16 Int Int
    |I64Store32 Int Int
    |CurrentMemory Int
    |GrowMemory Int
    |I32Const Int
    |I64Const Int
    |F32Const Float
    |F64Const Float
    |I32Eqz
    |I32Eq
    |I32Ne
    |I32Lts
    |I32Ltu
    |I32Gts
    |I32Gtu
    |I32Les
    |I32Leu
    |I32Ges
    |I32Geu
    |I64Eqz
    |I64Eq
    |I64Ne
    |I64Lts
    |I64Ltu
    |I64Gts
    |I64Gtu
    |I64Les
    |I64Leu
    |I64Ges
    |I64Geu
    |F32Eq
    |F32Ne
    |F32Lt
    |F32Gt
    |F32Le
    |F32Ge
    |F64Eq
    |F64Ne
    |F64Lt
    |F64Gt
    |F64Le
    |F64Ge
