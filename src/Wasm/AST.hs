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
