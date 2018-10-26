{-# LANGUAGE TemplateHaskell #-}

module Wasm.AST where
import qualified Data.ByteString               as BS
import           Data.Int
import           Control.Lens

data ValueType = ValI32|ValI64|ValF32|ValF64

data BlockType = BlockType (Maybe ValueType)

data ElemType = ElAnyFunc

data FuncType = FuncType [ValueType] (Maybe ValueType)

data LanguageType = LangValueType ValueType|LangElemType ElemType|LangFuncType FuncType|LangBlockType BlockType

data GlobalType = GlobalType ValueType Bool

data TableType = TableType ElemType ResizableLimits

data MemoryType = MemoryType ResizableLimits

data ExternalKind = ExFunction|ExTable|ExMemory|ExGlobal

data ExternalKindImport = ExImFunction Int|ExImExTable TableType|ExImExMemory MemoryType|ExImExGlobal GlobalType

data ResizableLimits = ResizableLimits Int (Maybe Int)

data InitExpr = InitI32 Int|InitI64 Int|InitF32 Float|InitF64 Double|InitGlobal Int

data TypeSection = TypeSection [FuncType]

data ImportEntry = ImportEntry String String ExternalKindImport

data ImportSection = ImportSection [ImportEntry]

data FunctionSection = FunctionSection [Int]

data TableSection = TableSection [TableType]

data MemorySection = MemorySection [MemoryType]

data GlobalSection = GlobalSection [GlobalVariable]

data GlobalVariable = GlobalVariable GlobalType InitExpr

data ExportSection = ExportSection [ExportEntry]

data  ExportEntry = ExportEntry String ExternalKind Int

data StartSection =  StartSection Int

data ElementSection = ElementSection [ElemSegment]

data ElemSegment = ElemSegment InitExpr [Int]

data CodeSection = CodeSection [FunctionBody]

data FunctionBody = FunctionBody [LocalEntry] [OperatorCode]

data LocalEntry = LocalEntry Int ValueType

data DataSection = DataSection [DataSegment]

data DataSegment = DataSegment InitExpr BS.ByteString

data MemoryImmediate = MemoryImmediate Int Int

data OperatorCode =
    OpUnreachable
    |OpNop
    |OpBlock BlockType
    |OpLoop BlockType
    |OpIf BlockType
    |OpElse
    |OpEnd
    |OpBr Int
    |OpBrIf Int
    |OpBrTable [Int] Int
    |OpReturn
    |OpCall Int
    |OpCallIndirect Int
    |OpDrop
    |OpSelect
    |OpGetLocal Int
    |OpSetLocal Int
    |OpTeeLocal Int
    |OpGetGlobal Int
    |OpSetGlobal Int
    |OpI32Load MemoryImmediate
    |OpI64Load MemoryImmediate
    |OpF32Load MemoryImmediate
    |OpF64Load MemoryImmediate
    |OpI32Load8s MemoryImmediate
    |OpI32Load8u MemoryImmediate
    |OpI32Load16s MemoryImmediate
    |OpI32Load16u MemoryImmediate
    |OpI64Load8s MemoryImmediate
    |OpI64Load8u MemoryImmediate
    |OpI64Load16s MemoryImmediate
    |OpI64Load16u MemoryImmediate
    |OpI64Load32s MemoryImmediate
    |OpI64Load32u MemoryImmediate
    |OpI32Store MemoryImmediate
    |OpI64Store MemoryImmediate
    |OpF32Store MemoryImmediate
    |OpF64Store MemoryImmediate
    |OpI32Store8 MemoryImmediate
    |OpI32Store16 MemoryImmediate
    |OpI64Store8 MemoryImmediate
    |OpI64Store16 MemoryImmediate
    |OpI64Store32 MemoryImmediate
    |OpCurrentMemory
    |OpGrowMemory
    |OpI32Const Int
    |OpI64Const Int
    |OpF32Const Float
    |OpF64Const Double
    |OpI32Eqz
    |OpI32Eq
    |OpI32Ne
    |OpI32Lts
    |OpI32Ltu
    |OpI32Gts
    |OpI32Gtu
    |OpI32Les
    |OpI32Leu
    |OpI32Ges
    |OpI32Geu
    |OpI64Eqz
    |OpI64Eq
    |OpI64Ne
    |OpI64Lts
    |OpI64Ltu
    |OpI64Gts
    |OpI64Gtu
    |OpI64Les
    |OpI64Leu
    |OpI64Ges
    |OpI64Geu
    |OpF32Eq
    |OpF32Ne
    |OpF32Lt
    |OpF32Gt
    |OpF32Le
    |OpF32Ge
    |OpF64Eq
    |OpF64Ne
    |OpF64Lt
    |OpF64Gt
    |OpF64Le
    |OpF64Ge
    |OpI32Clz
    |OpI32Ctz
    |OpI32Popcnt
    |OpI32Add
    |OpI32Sub
    |OpI32Mul
    |OpI32Divs
    |OpI32Divu
    |OpI32Rems
    |OpI32Remu
    |OpI32And
    |OpI32Or
    |OpI32Xor
    |OpI32Shl
    |OpI32Shrs
    |OpI32Shru
    |OpI32Rotl
    |OpI32Rotr
    |OpI64Clz
    |OpI64Ctz
    |OpI64Popcnt
    |OpI64Add
    |OpI64Sub
    |OpI64Mul
    |OpI64Divs
    |OpI64Divu
    |OpI64Rems
    |OpI64Remu
    |OpI64And
    |OpI64Or
    |OpI64Xor
    |OpI64Shl
    |OpI64Shrs
    |OpI64Shru
    |OpI64Rotl
    |OpI64Rotr
    |OpF32Abs
    |OpF32Neg
    |OpF32Ceil
    |OpF32Floor
    |OpF32Trunc
    |OpF32Nearest
    |OpF32Sqrt
    |OpF32Add
    |OpF32Sub
    |OpF32Mul
    |OpF32Div
    |OpF32Min
    |OpF32Max
    |OpF32Copysign
    |OpF64Abs
    |OpF64Neg
    |OpF64Ceil
    |OpF64Floor
    |OpF64Trunc
    |OpF64Nearest
    |OpF64Sqrt
    |OpF64Add
    |OpF64Sub
    |OpF64Mul
    |OpF64Div
    |OpF64Min
    |OpF64Max
    |OpF64Copysign
    |OpI32WrapI64
    |OpI32TruncsF32
    |OpI32TrancuF32
    |OpI32TrancsF64
    |OpI32TrancuF64
    |OpI64ExtendsI32
    |OpI64ExtenduI32
    |OpI64TruncsF32
    |OpI64TrancuF32
    |OpI64TrancsF64
    |OpI64TrancuF64
    |OpF32ConvertsI32
    |OpF32ConvertuI32
    |OpF32ConvertsI64
    |OpF32ConvertuI64
    |OpF32DemoteF64
    |OpF64ConvertsI32
    |OpF64ConvertuI32
    |OpF64ConvertsI64
    |OpF64ConvertuI64
    |OpF64PromoteF32
    |OpI32ReinterpretF32
    |OpI64ReinterpretF64
    |OpF32ReinterpretI32
    |OpF64ReinterpretI64

data WasmASTRoot = WasmASTRoot{
    _typeSection::Maybe TypeSection,
    _importSection::Maybe ImportSection,
    _functionSection::Maybe FunctionSection,
    _tableSection::Maybe TableSection,
    _memorySection:: Maybe MemorySection,
    _globalSection::Maybe GlobalSection,
    _exportSection:: Maybe ExportSection,
    _startSection::   Maybe StartSection,
    _elementSection:: Maybe ElementSection,
    _codeSection::  Maybe CodeSection,
    _dataSection::Maybe DataSection
}

makeLenses ''WasmASTRoot

wasmASTRootDefault :: WasmASTRoot
wasmASTRootDefault = WasmASTRoot
    { _typeSection     = Nothing
    , _importSection   = Nothing
    , _functionSection = Nothing
    , _tableSection    = Nothing
    , _memorySection   = Nothing
    , _globalSection   = Nothing
    , _exportSection   = Nothing
    , _startSection    = Nothing
    , _elementSection  = Nothing
    , _codeSection     = Nothing
    , _dataSection     = Nothing
    }
