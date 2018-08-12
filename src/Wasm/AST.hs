module Wasm.AST where
import qualified Data.Binary                   as B
import qualified Data.ByteString               as BS
import           Data.Int
import           Data.Binary.Get

data ValueType = I32|I64|F32|F64

data ElemType=AnyFunc

data Type = ValueType ValueType|ElemType ElemType|Func FuncType|BlockType BlockType

type BlockType = Maybe ValueType

type FuncType=(Int,[ValueType],Maybe ValueType)

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
    |I32Clz
    |I32Ctz
    |I32Popcnt
    |I32Add
    |I32Sub
    |I32Mul
    |I32Divs
    |I32Divu
    |I32Rems
    |I32Remu
    |I32And
    |I32Or
    |I32Xor
    |I32Shl
    |I32Shrs
    |I32Shru
    |I32Rotl
    |I32Rotr
    |I64Clz
    |I64Ctz
    |I64Popcnt
    |I64Add
    |I64Sub
    |I64Mul
    |I64Divs
    |I64Divu
    |I64Rems
    |I64Remu
    |I64And
    |I64Or
    |I64Xor
    |I64Shl
    |I64Shrs
    |I64Shru
    |I64Rotl
    |I64Rotr
    |F32Abs
    |F32Neg
    |F32Ceil
    |F32Floor
    |F32Trunc
    |F32Nearest
    |F32Sqrt
    |F32Add
    |F32Sub
    |F32Mul
    |F32Div
    |F32Min
    |F32Max
    |F32Copysign
    |F64Abs
    |F64Neg
    |F64Ceil
    |F64Floor
    |F64Trunc
    |F64Nearest
    |F64Sqrt
    |F64Add
    |F64Sub
    |F64Mul
    |F64Div
    |F64Min
    |F64Max
    |F64Copysign
    |I32WrapI64
    |I32TruncsF32
    |I32TrancuF32
    |I32TrancsF64
    |I32TrancuF64
    |I64ExtendsI32
    |I64ExtenduI32
    |I64TruncsF32
    |I64TrancuF32
    |I64TrancsF64
    |I64TrancuF64
    |F32ConvertsI32
    |F32ConvertuI32
    |F32ConvertsI64
    |F32ConvertuI64
    |F32DemoteF64
    |F64ConvertsI32
    |F64ConvertuI32
    |F64ConvertsI64
    |F64ConvertuI64
    |F64PromoteF32
    |I32ReinterpretF32
    |I64ReinterpretF64
    |F32ReinterpretI32
    |F64ReinterpretI64

type ResizableLimits=(Int,Maybe Int)

type TableType=(ElemType,ResizableLimits)

type MemoryType=ResizableLimits

type GlobalType=(ValueType,Bool)

data ExternalKind=EFunction|ETable|EMemory|EGlobal

type Import=(String ,String ,ExternalKind)

type GlobalVariable=(GlobalType,InitExpr)

data InitExpr=InitI32 Int|InitI64 Int|InitF32 Float|InitF64 Float|InitGlobal Int

type ExportType=(String,ExternalKind,Int)

type FunctionBody=([LocalEntry],[FuncCmd])

type LocalEntry=(Int,ValueType)

type ElemSegment=(Int,Int,[Int])

type DataSegment=(Int,InitExpr,BS.ByteString)

data Section=Section{
    types::[FuncType],
    imports::[Import],
    funcs::[Int],
    tables::[TableType],
    memorys::[MemoryType],
    globals::[GlobalVariable],
    exports::[ExportType],
    start::Int,
    elems::[ElemSegment],
    codes::[FunctionBody],
    datas::[DataSegment]
}
