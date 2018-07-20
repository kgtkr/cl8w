module Wasm.AST where
import qualified Data.Binary                   as B
import qualified Data.ByteString               as BS
import           Data.Int
import           Data.Binary.Get

data ValueType=I32|I64|F32|F64

data Type=ValueType ValueType|AnyFunc|Func|BlockType (Maybe ValueType)

type BlockType=Maybe ValueType

data FuncCmd=
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


