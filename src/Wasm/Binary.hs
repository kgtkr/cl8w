module Wasm.Binary where

import           Wasm.AST
import           Data.Int
import           Data.Word
import qualified Data.Bytes.Signed             as S
import           Data.Serialize.Put
import           Data.Serialize
import           Data.Bytes.VarInt
import qualified Data.Bytes.Put                as P
import           Data.Bytes.Serial              ( serialize )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as BSU

putUint8 :: Putter Int
putUint8 = putWord8 . fromIntegral

putUint16 :: Putter Int
putUint16 = putWord16le . fromIntegral

putUint32 :: Putter Int
putUint32 = putWord32le . fromIntegral

putVaruint1 :: Putter Bool
putVaruint1 True  = serialize (1 :: VarInt Word8)
putVaruint1 False = serialize (0 :: VarInt Word8)

putVaruint7 :: Putter Int
putVaruint7 x = serialize (fromIntegral x :: VarInt Word8)

putVaruint32 :: Putter Int
putVaruint32 x = serialize (fromIntegral x :: VarInt Word32)

putVarint7 :: Putter Int
putVarint7 x = serialize (fromIntegral x :: VarInt Int8)

putVarint32 :: Putter Int
putVarint32 x = serialize (fromIntegral x :: VarInt Int32)

putVarint64 :: Putter Int
putVarint64 x = serialize (fromIntegral x :: VarInt Int64)

putFloat32 :: Putter Float
putFloat32 = putFloat32le

putFloat64 :: Putter Double
putFloat64 = putFloat64le

putArray :: (a -> Put) -> [a] -> Put
putArray f x = do
    putVaruint32 $ length x
    mapM_ f x

putArrayAST :: (WasmAST a) => [a] -> Put
putArrayAST = putArray putWasmAST

putBytes :: Putter BS.ByteString
putBytes x = do
    putVaruint32 $ BS.length x
    putByteString x

putString :: Putter String
putString = (putBytes . BSU.fromString)

class WasmAST a where
    putWasmAST::Putter a


putSction :: Int -> Put -> Put
putSction id body = do
    putVaruint7 $ id
    putBytes $ runPut body

instance WasmAST ValueType where
    putWasmAST ValI32 = putVarint7 (-0x01)
    putWasmAST ValI64 = putVarint7 (-0x02)
    putWasmAST ValF32 = putVarint7 (-0x03)
    putWasmAST ValF64 = putVarint7 (-0x04)

instance WasmAST BlockType where
    putWasmAST (BlockType (Just x)) = putWasmAST x
    putWasmAST (BlockType Nothing)  = putVarint7 (-0x40)

instance WasmAST ElemType where
    putWasmAST ElAnyFunc = putVarint7 (-0x10)

instance WasmAST FuncType where
    putWasmAST (FuncType p r) = do
        putVarint7 (-0x20)
        putArrayAST p
        case r of
            Just r->do
                putVaruint1 True
                putWasmAST r
            Nothing->putVaruint1 False

instance WasmAST LanguageType where
    putWasmAST (LangValueType x)=putWasmAST x
    putWasmAST (LangElemType x)=putWasmAST x
    putWasmAST (LangFuncType x)=putWasmAST x
    putWasmAST (LangBlockType x)=putWasmAST x

instance WasmAST GlobalType where
    putWasmAST (GlobalType x y)=do
        putWasmAST x
        putVaruint1 y

instance WasmAST TableType where
    putWasmAST (TableType x y)=do
        putWasmAST x
        putWasmAST y

instance WasmAST MemoryType where
    putWasmAST (MemoryType x)=putWasmAST x

instance WasmAST ResizableLimits where
    putWasmAST (ResizableLimits x (Just y))=do
        putVaruint1 True
        putVaruint32 x
        putVaruint32 y
    putWasmAST (ResizableLimits x Nothing)=do
        putVaruint1 False
        putVaruint32 x

instance WasmAST ExternalKind where
    putWasmAST ExFunction=putUint8 0
    putWasmAST ExTable=putUint8 1
    putWasmAST ExMemory=putUint8 2
    putWasmAST ExGlobal=putUint8 3

instance WasmAST InitExpr where
    putWasmAST=undefined
instance WasmAST TypeSection where
    putWasmAST (TypeSection x)=putSction 1 (putArrayAST x)
instance WasmAST ExternalKindImport where
    putWasmAST (ExImFunction x)=do
        putUint8 0
        putVaruint32 x

    putWasmAST (ExImExTable x)=do
        putUint8 1
        putWasmAST x

    putWasmAST (ExImExMemory x)=do
        putUint8 2
        putWasmAST x

    putWasmAST (ExImExGlobal x)=do
        putUint8 3
        putWasmAST x

instance WasmAST ImportEntry where
    putWasmAST (ImportEntry x y z)=do
        putString x
        putString y
        putWasmAST z

instance WasmAST ImportSection where
    putWasmAST (ImportSection x)=putSction 2 (putArrayAST x)

instance WasmAST FunctionSection where
    putWasmAST (FunctionSection x)=putSction 3 (putArray putVaruint32 x)

instance WasmAST TableSection where
    putWasmAST (TableSection x)=putSction 4 (putArrayAST x)

instance WasmAST MemorySection where
    putWasmAST (MemorySection x)=putSction 5 (putArrayAST x)

instance WasmAST GlobalSection where
    putWasmAST (GlobalSection x)=putSction 6 (putArrayAST x)

instance WasmAST GlobalVariable where
    putWasmAST (GlobalVariable x y)=do
        putWasmAST x
        putWasmAST y

instance WasmAST ExportSection where
    putWasmAST (ExportSection x)=putSction 7 (putArrayAST x)

instance WasmAST ExportEntry where
    putWasmAST (ExportEntry x y z)=do
        putString x
        putWasmAST y
        putVaruint32 z

instance WasmAST StartSection where
    putWasmAST (StartSection x)=putSction 8 (putVaruint32 x)

instance WasmAST ElementSection where
    putWasmAST (ElementSection x)=putSction 9 (putArrayAST x)

instance WasmAST ElemSegment where
    putWasmAST (ElemSegment x y)=do
        putVaruint32 0
        putWasmAST x
        putArray putVaruint32 y

instance WasmAST CodeSection where
    putWasmAST (CodeSection x)=putSction 10 (putArrayAST x)

instance WasmAST FunctionBody where
    putWasmAST (FunctionBody x y)=do
        let body=runPut $ do
                    putArrayAST x
                    mapM_ putWasmAST y
                    putWasmAST OpEnd
        putBytes body


instance WasmAST LocalEntry where
    putWasmAST (LocalEntry x y)=do
        putVaruint32 x
        putWasmAST y

instance WasmAST DataSection where
    putWasmAST (DataSection x)=putSction 11 (putArrayAST x)

instance WasmAST DataSegment where
    putWasmAST (DataSegment x y)=do
        putVaruint32 0
        putWasmAST x
        putBytes y

instance WasmAST MemoryImmediate where
    putWasmAST (MemoryImmediate x y)=do
        putVaruint32 x
        putVaruint32 y

instance WasmAST OperatorCode where
    putWasmAST OpUnreachable=putUint32 0x00
    putWasmAST OpNop=putUint32 0x01
    putWasmAST (OpBlock x)=do
        putUint32 0x02
        putWasmAST x
    putWasmAST (OpLoop x)=do
        putUint32 0x03
        putWasmAST x
    putWasmAST (OpIf x)=do
        putUint32 0x04
        putWasmAST x
    putWasmAST OpElse=putUint32 0x05
    putWasmAST OpEnd=putUint32 0x0b
    putWasmAST (OpBr x)=do
        putUint32 0x0c
        putVaruint32 x
    putWasmAST (OpBrIf x)=do
        putUint32 0x0d
        putVaruint32 x
    putWasmAST (OpBrTable x y)=do
        putUint32 0x0e
        putArray putVaruint32 x
        putVaruint32 y
    putWasmAST OpReturn=putUint32 0x0f
    putWasmAST (OpCall x)=do
        putUint32 0x10
        putVaruint32 x
    putWasmAST (OpCallIndirect x)=do
        putUint32 0x11
        putVaruint32 x
        putVaruint1 False
    putWasmAST OpDrop=putUint32 0x1a
    putWasmAST OpSelect=putUint32 0x1b
    putWasmAST (OpGetLocal x)=do
        putUint32 0x20
        putVaruint32 x
    putWasmAST (OpSetLocal x)=do
        putUint32 0x21
        putVaruint32 x
    putWasmAST (OpTeeLocal x)=do
        putUint32 0x22
        putVaruint32 x
    putWasmAST (OpGetGlobal x)=do
        putUint32 0x23
        putVaruint32 x
    putWasmAST (OpSetGlobal x)=do
        putUint32 0x24
        putVaruint32 x
    putWasmAST (OpI32Load x)=do
        putUint32 0x28
        putWasmAST x
    putWasmAST (OpI64Load x)=do
        putUint32 0x29
        putWasmAST x
    putWasmAST (OpF32Load x)=do
        putUint32 0x2a
        putWasmAST x
    putWasmAST (OpF64Load x)=do
        putUint32 0x2b
        putWasmAST x
    putWasmAST (OpI32Load8s x)=do
        putUint32 0x2c
        putWasmAST x
    putWasmAST (OpI32Load8u x)=do
        putUint32 0x2d
        putWasmAST x
    putWasmAST (OpI32Load16s x)=do
        putUint32 0x2e
        putWasmAST x
    putWasmAST (OpI32Load16u x)=do
        putUint32 0x2f
        putWasmAST x
    putWasmAST (OpI64Load8s x)=do
        putUint32 0x30
        putWasmAST x
    putWasmAST (OpI64Load8u x)=do
        putUint32 0x31
        putWasmAST x
    putWasmAST (OpI64Load16s x)=do
        putUint32 0x32
        putWasmAST x
    putWasmAST (OpI64Load16u x)=do
        putUint32 0x33
        putWasmAST x
    putWasmAST (OpI64Load32s x)=do
        putUint32 0x34
        putWasmAST x
    putWasmAST (OpI64Load32u x)=do
        putUint32 0x35
        putWasmAST x
    putWasmAST (OpI32Store x)=do
        putUint32 0x36
        putWasmAST x
    putWasmAST (OpI64Store x)=do
        putUint32 0x37
        putWasmAST x
    putWasmAST (OpF32Store x)=do
        putUint32 0x38
        putWasmAST x
    putWasmAST (OpF64Store x)=do
        putUint32 0x39
        putWasmAST x
    putWasmAST (OpI32Store8 x)=do
        putUint32 0x3a
        putWasmAST x
    putWasmAST (OpI32Store16 x)=do
        putUint32 0x3b
        putWasmAST x
    putWasmAST (OpI64Store8 x)=do
        putUint32 0x3c
        putWasmAST x
    putWasmAST (OpI64Store16 x)=do
        putUint32 0x3d
        putWasmAST x
    putWasmAST (OpI64Store32 x)=do
        putUint32 0x3e
        putWasmAST x
    putWasmAST OpCurrentMemory=do
        putUint32 0x3f
        putVaruint1 False
    putWasmAST OpGrowMemory=do
        putUint32 0x40
        putVaruint1 False
    putWasmAST (OpI32Const x)=do
        putUint32 0x41
        putVarint32 x
    putWasmAST (OpI64Const x)=do
        putUint32 0x42
        putVarint64 x
    putWasmAST (OpF32Const x)=do
        putUint32 0x43
        putFloat32 x
    putWasmAST (OpF64Const x)=do
        putUint32 0x44
        putFloat64 x
    putWasmAST OpI32Eqz=putUint32 0x45
    putWasmAST OpI32Eq=putUint32 0x46
    putWasmAST OpI32Ne=putUint32 0x47
    putWasmAST OpI32Lts=putUint32 0x48
    putWasmAST OpI32Ltu=putUint32 0x49
    putWasmAST OpI32Gts=putUint32 0x4a
    putWasmAST OpI32Gtu=putUint32 0x4b
    putWasmAST OpI32Les=putUint32 0x4c
    putWasmAST OpI32Leu=putUint32 0x4d
    putWasmAST OpI32Ges=putUint32 0x4e
    putWasmAST OpI32Geu=putUint32 0x4f
    putWasmAST OpI64Eqz=putUint32 0x50
    putWasmAST OpI64Eq=putUint32 0x51
    putWasmAST OpI64Ne=putUint32 0x52
    putWasmAST OpI64Lts=putUint32 0x53
    putWasmAST OpI64Ltu=putUint32 0x54
    putWasmAST OpI64Gts=putUint32 0x55
    putWasmAST OpI64Gtu=putUint32 0x56
    putWasmAST OpI64Les=putUint32 0x57
    putWasmAST OpI64Leu=putUint32 0x58
    putWasmAST OpI64Ges=putUint32 0x59
    putWasmAST OpI64Geu=putUint32 0x5a
    putWasmAST OpF32Eq=putUint32 0x5b
    putWasmAST OpF32Ne=putUint32 0x5c
    putWasmAST OpF32Lt=putUint32 0x5d
    putWasmAST OpF32Gt=putUint32 0x5e
    putWasmAST OpF32Le=putUint32 0x5f
    putWasmAST OpF32Ge=putUint32 0x60
    putWasmAST OpF64Eq=putUint32 0x61
    putWasmAST OpF64Ne=putUint32 0x62
    putWasmAST OpF64Lt=putUint32 0x63
    putWasmAST OpF64Gt=putUint32 0x64
    putWasmAST OpF64Le=putUint32 0x65
    putWasmAST OpF64Ge=putUint32 0x66
    putWasmAST OpI32Clz=putUint32 0x67
    putWasmAST OpI32Ctz=putUint32 0x68
    putWasmAST OpI32Popcnt=putUint32 0x69
    putWasmAST OpI32Add=putUint32 0x6a
    putWasmAST OpI32Sub=putUint32 0x6b
    putWasmAST OpI32Mul=putUint32 0x6c
    putWasmAST OpI32Divs=putUint32 0x6d
    putWasmAST OpI32Divu=putUint32 0x6e
    putWasmAST OpI32Rems=putUint32 0x6f
    putWasmAST OpI32Remu=putUint32 0x70
    putWasmAST OpI32And=putUint32 0x71
    putWasmAST OpI32Or=putUint32 0x72
    putWasmAST OpI32Xor=putUint32 0x73
    putWasmAST OpI32Shl=putUint32 0x74
    putWasmAST OpI32Shrs=putUint32 0x75
    putWasmAST OpI32Shru=putUint32 0x76
    putWasmAST OpI32Rotl=putUint32 0x77
    putWasmAST OpI32Rotr=putUint32 0x78
    putWasmAST OpI64Clz=putUint32 0x79
    putWasmAST OpI64Ctz=putUint32 0x7a
    putWasmAST OpI64Popcnt=putUint32 0x7b
    putWasmAST OpI64Add=putUint32 0x7c
    putWasmAST OpI64Sub=putUint32 0x7d
    putWasmAST OpI64Mul=putUint32 0x7e
    putWasmAST OpI64Divs=putUint32 0x7f
    putWasmAST OpI64Divu=putUint32 0x80
    putWasmAST OpI64Rems=putUint32 0x81
    putWasmAST OpI64Remu=putUint32 0x82
    putWasmAST OpI64And=putUint32 0x83
    putWasmAST OpI64Or=putUint32 0x84
    putWasmAST OpI64Xor=putUint32 0x85
    putWasmAST OpI64Shl=putUint32 0x86
    putWasmAST OpI64Shrs=putUint32 0x87
    putWasmAST OpI64Shru=putUint32 0x88
    putWasmAST OpI64Rotl=putUint32 0x89
    putWasmAST OpI64Rotr=putUint32 0x8a
    putWasmAST OpF32Abs=putUint32 0x8b
    putWasmAST OpF32Neg=putUint32 0x8c
    putWasmAST OpF32Ceil=putUint32 0x8d
    putWasmAST OpF32Floor=putUint32 0x8e
    putWasmAST OpF32Trunc=putUint32 0x8f
    putWasmAST OpF32Nearest=putUint32 0x90
    putWasmAST OpF32Sqrt=putUint32 0x91
    putWasmAST OpF32Add=putUint32 0x92
    putWasmAST OpF32Sub=putUint32 0x93
    putWasmAST OpF32Mul=putUint32 0x94
    putWasmAST OpF32Div=putUint32 0x95
    putWasmAST OpF32Min=putUint32 0x96
    putWasmAST OpF32Max=putUint32 0x97
    putWasmAST OpF32Copysign=putUint32 0x98
    putWasmAST OpF64Abs=putUint32 0x99
    putWasmAST OpF64Neg=putUint32 0x9a
    putWasmAST OpF64Ceil=putUint32 0x9b
    putWasmAST OpF64Floor=putUint32 0x9c
    putWasmAST OpF64Trunc=putUint32 0x9d
    putWasmAST OpF64Nearest=putUint32 0x9e
    putWasmAST OpF64Sqrt=putUint32 0x9f
    putWasmAST OpF64Add=putUint32 0xa0
    putWasmAST OpF64Sub=putUint32 0xa1
    putWasmAST OpF64Mul=putUint32 0xa2
    putWasmAST OpF64Div=putUint32 0xa3
    putWasmAST OpF64Min=putUint32 0xa4
    putWasmAST OpF64Max=putUint32 0xa5
    putWasmAST OpF64Copysign=putUint32 0xa6
    putWasmAST OpI32WrapI64=putUint32 0xa7
    putWasmAST OpI32TruncsF32=putUint32 0xa8
    putWasmAST OpI32TrancuF32=putUint32 0xa9
    putWasmAST OpI32TrancsF64=putUint32 0xaa
    putWasmAST OpI32TrancuF64=putUint32 0xab
    putWasmAST OpI64ExtendsI32=putUint32 0xac
    putWasmAST OpI64ExtenduI32=putUint32 0xad
    putWasmAST OpI64TruncsF32=putUint32 0xae
    putWasmAST OpI64TrancuF32=putUint32 0xaf
    putWasmAST OpI64TrancsF64=putUint32 0xb0
    putWasmAST OpI64TrancuF64=putUint32 0xb1
    putWasmAST OpF32ConvertsI32=putUint32 0xb2
    putWasmAST OpF32ConvertuI32=putUint32 0xb3
    putWasmAST OpF32ConvertsI64=putUint32 0xb4
    putWasmAST OpF32ConvertuI64=putUint32 0xb5
    putWasmAST OpF32DemoteF64=putUint32 0xb6
    putWasmAST OpF64ConvertsI32=putUint32 0xb7
    putWasmAST OpF64ConvertuI32=putUint32 0xb8
    putWasmAST OpF64ConvertsI64=putUint32 0xb9
    putWasmAST OpF64ConvertuI64=putUint32 0xba
    putWasmAST OpF64PromoteF32=putUint32 0xbb
