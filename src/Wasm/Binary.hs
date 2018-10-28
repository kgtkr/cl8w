module Wasm.Binary where

import qualified Wasm.AST                      as WA
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
import           Control.Lens
import           Data.Bits

putSleb128 :: Putter Int
putSleb128 x
    | (v == 0 && b .&. 0x40 == 0) || (v == -1 && b .&. 0x40 /= 0)
    = (putWord8 . fromIntegral) b
    | otherwise
    = do
        (putWord8 . fromIntegral) (b .|. 0x80)
        putSleb128 v
  where
    b = x .&. 0x7f
    v = x `shiftR` 7

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
putVarint32 x = putSleb128 x

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
    putVaruint32 $ id
    putBytes $ runPut body

putMaybe :: (WasmAST a) => Maybe a -> Put
putMaybe Nothing  = return ()
putMaybe (Just x) = putWasmAST x

instance WasmAST WA.ValueType where
    putWasmAST WA.ValI32 = putVarint7 0x7f
    putWasmAST WA.ValI64 = putVarint7 0x7e
    putWasmAST WA.ValF32 = putVarint7 0x7d
    putWasmAST WA.ValF64 = putVarint7 0x7c

instance WasmAST WA.BlockType where
    putWasmAST (WA.BlockType (Just x)) = putWasmAST x
    putWasmAST (WA.BlockType Nothing)  = putVarint7 0x40

instance WasmAST WA.ElemType where
    putWasmAST WA.ElAnyFunc = putVarint7 0x7c

instance WasmAST WA.FuncType where
    putWasmAST (WA.FuncType p r) = do
        putVarint7 0x60
        putArrayAST p
        case r of
            Just r->do
                putVaruint1 True
                putWasmAST r
            Nothing->putVaruint1 False

instance WasmAST WA.LanguageType where
    putWasmAST (WA.LangValueType x)=putWasmAST x
    putWasmAST (WA.LangElemType x)=putWasmAST x
    putWasmAST (WA.LangFuncType x)=putWasmAST x
    putWasmAST (WA.LangBlockType x)=putWasmAST x

instance WasmAST WA.GlobalType where
    putWasmAST (WA.GlobalType x y)=do
        putWasmAST x
        putVaruint1 y

instance WasmAST WA.TableType where
    putWasmAST (WA.TableType x y)=do
        putWasmAST x
        putWasmAST y

instance WasmAST WA.MemoryType where
    putWasmAST (WA.MemoryType x)=putWasmAST x

instance WasmAST WA.ResizableLimits where
    putWasmAST (WA.ResizableLimits x (Just y))=do
        putVaruint1 True
        putVaruint32 x
        putVaruint32 y
    putWasmAST (WA.ResizableLimits x Nothing)=do
        putVaruint1 False
        putVaruint32 x

instance WasmAST WA.ExternalKind where
    putWasmAST WA.ExFunction=putUint8 0
    putWasmAST WA.ExTable=putUint8 1
    putWasmAST WA.ExMemory=putUint8 2
    putWasmAST WA.ExGlobal=putUint8 3

instance WasmAST WA.InitExpr where
    putWasmAST (WA.InitI32 x)=putWasmAST (WA.OpI32Const x)
    putWasmAST (WA.InitI64 x)=putWasmAST (WA.OpI64Const x)
    putWasmAST (WA.InitF32 x)=putWasmAST (WA.OpF32Const x)
    putWasmAST (WA.InitF64 x)=putWasmAST (WA.OpF64Const x)
    putWasmAST (WA.InitGlobal x)=putWasmAST (WA.OpGetGlobal x)
instance WasmAST WA.TypeSection where
    putWasmAST (WA.TypeSection x)=putSction 1 (putArrayAST x)
instance WasmAST WA.ExternalKindImport where
    putWasmAST (WA.ExImFunction x)=do
        putUint8 0
        putVaruint32 x

    putWasmAST (WA.ExImExTable x)=do
        putUint8 1
        putWasmAST x

    putWasmAST (WA.ExImExMemory x)=do
        putUint8 2
        putWasmAST x

    putWasmAST (WA.ExImExGlobal x)=do
        putUint8 3
        putWasmAST x

instance WasmAST WA.ImportEntry where
    putWasmAST (WA.ImportEntry x y z)=do
        putString x
        putString y
        putWasmAST z

instance WasmAST WA.ImportSection where
    putWasmAST (WA.ImportSection x)=putSction 2 (putArrayAST x)

instance WasmAST WA.FunctionSection where
    putWasmAST (WA.FunctionSection x)=putSction 3 (putArray putVaruint32 x)

instance WasmAST WA.TableSection where
    putWasmAST (WA.TableSection x)=putSction 4 (putArrayAST x)

instance WasmAST WA.MemorySection where
    putWasmAST (WA.MemorySection x)=putSction 5 (putArrayAST x)

instance WasmAST WA.GlobalSection where
    putWasmAST (WA.GlobalSection x)=putSction 6 (putArrayAST x)

instance WasmAST WA.GlobalVariable where
    putWasmAST (WA.GlobalVariable x y)=do
        putWasmAST x
        putWasmAST y

instance WasmAST WA.ExportSection where
    putWasmAST (WA.ExportSection x)=putSction 7 (putArrayAST x)

instance WasmAST WA.ExportEntry where
    putWasmAST (WA.ExportEntry x y z)=do
        putString x
        putWasmAST y
        putVaruint32 z

instance WasmAST WA.StartSection where
    putWasmAST (WA.StartSection x)=putSction 8 (putVaruint32 x)

instance WasmAST WA.ElementSection where
    putWasmAST (WA.ElementSection x)=putSction 9 (putArrayAST x)

instance WasmAST WA.ElemSegment where
    putWasmAST (WA.ElemSegment x y)=do
        putVaruint32 0
        putWasmAST x
        putArray putVaruint32 y

instance WasmAST WA.CodeSection where
    putWasmAST (WA.CodeSection x)=putSction 10 (putArrayAST x)

instance WasmAST WA.FunctionBody where
    putWasmAST (WA.FunctionBody x y)=do
        let body=runPut $ do
                    putArrayAST x
                    mapM_ putWasmAST y
                    putWasmAST WA.OpEnd
        putBytes body


instance WasmAST WA.LocalEntry where
    putWasmAST (WA.LocalEntry x y)=do
        putVaruint32 x
        putWasmAST y

instance WasmAST WA.DataSection where
    putWasmAST (WA.DataSection x)=putSction 11 (putArrayAST x)

instance WasmAST WA.DataSegment where
    putWasmAST (WA.DataSegment x y)=do
        putVaruint32 0
        putWasmAST x
        putBytes y

instance WasmAST WA.MemoryImmediate where
    putWasmAST (WA.MemoryImmediate x y)=do
        putVaruint32 x
        putVaruint32 y

putOpcode :: Putter Int
putOpcode = putUint8

instance WasmAST WA.OperatorCode where
    putWasmAST WA.OpUnreachable=putOpcode 0x00
    putWasmAST WA.OpNop=putOpcode 0x01
    putWasmAST (WA.OpBlock x)=do
        putOpcode 0x02
        putWasmAST x
    putWasmAST (WA.OpLoop x)=do
        putOpcode 0x03
        putWasmAST x
    putWasmAST (WA.OpIf x)=do
        putOpcode 0x04
        putWasmAST x
    putWasmAST WA.OpElse=putOpcode 0x05
    putWasmAST WA.OpEnd=putOpcode 0x0b
    putWasmAST (WA.OpBr x)=do
        putOpcode 0x0c
        putVaruint32 x
    putWasmAST (WA.OpBrIf x)=do
        putOpcode 0x0d
        putVaruint32 x
    putWasmAST (WA.OpBrTable x y)=do
        putOpcode 0x0e
        putArray putVaruint32 x
        putVaruint32 y
    putWasmAST WA.OpReturn=putOpcode 0x0f
    putWasmAST (WA.OpCall x)=do
        putOpcode 0x10
        putVaruint32 x
    putWasmAST (WA.OpCallIndirect x)=do
        putOpcode 0x11
        putVaruint32 x
        putVaruint1 False
    putWasmAST WA.OpDrop=putOpcode 0x1a
    putWasmAST WA.OpSelect=putOpcode 0x1b
    putWasmAST (WA.OpGetLocal x)=do
        putOpcode 0x20
        putVaruint32 x
    putWasmAST (WA.OpSetLocal x)=do
        putOpcode 0x21
        putVaruint32 x
    putWasmAST (WA.OpTeeLocal x)=do
        putOpcode 0x22
        putVaruint32 x
    putWasmAST (WA.OpGetGlobal x)=do
        putOpcode 0x23
        putVaruint32 x
    putWasmAST (WA.OpSetGlobal x)=do
        putOpcode 0x24
        putVaruint32 x
    putWasmAST (WA.OpI32Load x)=do
        putOpcode 0x28
        putWasmAST x
    putWasmAST (WA.OpI64Load x)=do
        putOpcode 0x29
        putWasmAST x
    putWasmAST (WA.OpF32Load x)=do
        putOpcode 0x2a
        putWasmAST x
    putWasmAST (WA.OpF64Load x)=do
        putOpcode 0x2b
        putWasmAST x
    putWasmAST (WA.OpI32Load8s x)=do
        putOpcode 0x2c
        putWasmAST x
    putWasmAST (WA.OpI32Load8u x)=do
        putOpcode 0x2d
        putWasmAST x
    putWasmAST (WA.OpI32Load16s x)=do
        putOpcode 0x2e
        putWasmAST x
    putWasmAST (WA.OpI32Load16u x)=do
        putOpcode 0x2f
        putWasmAST x
    putWasmAST (WA.OpI64Load8s x)=do
        putOpcode 0x30
        putWasmAST x
    putWasmAST (WA.OpI64Load8u x)=do
        putOpcode 0x31
        putWasmAST x
    putWasmAST (WA.OpI64Load16s x)=do
        putOpcode 0x32
        putWasmAST x
    putWasmAST (WA.OpI64Load16u x)=do
        putOpcode 0x33
        putWasmAST x
    putWasmAST (WA.OpI64Load32s x)=do
        putOpcode 0x34
        putWasmAST x
    putWasmAST (WA.OpI64Load32u x)=do
        putOpcode 0x35
        putWasmAST x
    putWasmAST (WA.OpI32Store x)=do
        putOpcode 0x36
        putWasmAST x
    putWasmAST (WA.OpI64Store x)=do
        putOpcode 0x37
        putWasmAST x
    putWasmAST (WA.OpF32Store x)=do
        putOpcode 0x38
        putWasmAST x
    putWasmAST (WA.OpF64Store x)=do
        putOpcode 0x39
        putWasmAST x
    putWasmAST (WA.OpI32Store8 x)=do
        putOpcode 0x3a
        putWasmAST x
    putWasmAST (WA.OpI32Store16 x)=do
        putOpcode 0x3b
        putWasmAST x
    putWasmAST (WA.OpI64Store8 x)=do
        putOpcode 0x3c
        putWasmAST x
    putWasmAST (WA.OpI64Store16 x)=do
        putOpcode 0x3d
        putWasmAST x
    putWasmAST (WA.OpI64Store32 x)=do
        putOpcode 0x3e
        putWasmAST x
    putWasmAST WA.OpCurrentMemory=do
        putOpcode 0x3f
        putVaruint1 False
    putWasmAST WA.OpGrowMemory=do
        putOpcode 0x40
        putVaruint1 False
    putWasmAST (WA.OpI32Const x)=do
        putOpcode 0x41
        putVarint32 x
    putWasmAST (WA.OpI64Const x)=do
        putOpcode 0x42
        putVarint64 x
    putWasmAST (WA.OpF32Const x)=do
        putOpcode 0x43
        putFloat32 x
    putWasmAST (WA.OpF64Const x)=do
        putOpcode 0x44
        putFloat64 x
    putWasmAST WA.OpI32Eqz=putOpcode 0x45
    putWasmAST WA.OpI32Eq=putOpcode 0x46
    putWasmAST WA.OpI32Ne=putOpcode 0x47
    putWasmAST WA.OpI32Lts=putOpcode 0x48
    putWasmAST WA.OpI32Ltu=putOpcode 0x49
    putWasmAST WA.OpI32Gts=putOpcode 0x4a
    putWasmAST WA.OpI32Gtu=putOpcode 0x4b
    putWasmAST WA.OpI32Les=putOpcode 0x4c
    putWasmAST WA.OpI32Leu=putOpcode 0x4d
    putWasmAST WA.OpI32Ges=putOpcode 0x4e
    putWasmAST WA.OpI32Geu=putOpcode 0x4f
    putWasmAST WA.OpI64Eqz=putOpcode 0x50
    putWasmAST WA.OpI64Eq=putOpcode 0x51
    putWasmAST WA.OpI64Ne=putOpcode 0x52
    putWasmAST WA.OpI64Lts=putOpcode 0x53
    putWasmAST WA.OpI64Ltu=putOpcode 0x54
    putWasmAST WA.OpI64Gts=putOpcode 0x55
    putWasmAST WA.OpI64Gtu=putOpcode 0x56
    putWasmAST WA.OpI64Les=putOpcode 0x57
    putWasmAST WA.OpI64Leu=putOpcode 0x58
    putWasmAST WA.OpI64Ges=putOpcode 0x59
    putWasmAST WA.OpI64Geu=putOpcode 0x5a
    putWasmAST WA.OpF32Eq=putOpcode 0x5b
    putWasmAST WA.OpF32Ne=putOpcode 0x5c
    putWasmAST WA.OpF32Lt=putOpcode 0x5d
    putWasmAST WA.OpF32Gt=putOpcode 0x5e
    putWasmAST WA.OpF32Le=putOpcode 0x5f
    putWasmAST WA.OpF32Ge=putOpcode 0x60
    putWasmAST WA.OpF64Eq=putOpcode 0x61
    putWasmAST WA.OpF64Ne=putOpcode 0x62
    putWasmAST WA.OpF64Lt=putOpcode 0x63
    putWasmAST WA.OpF64Gt=putOpcode 0x64
    putWasmAST WA.OpF64Le=putOpcode 0x65
    putWasmAST WA.OpF64Ge=putOpcode 0x66
    putWasmAST WA.OpI32Clz=putOpcode 0x67
    putWasmAST WA.OpI32Ctz=putOpcode 0x68
    putWasmAST WA.OpI32Popcnt=putOpcode 0x69
    putWasmAST WA.OpI32Add=putOpcode 0x6a
    putWasmAST WA.OpI32Sub=putOpcode 0x6b
    putWasmAST WA.OpI32Mul=putOpcode 0x6c
    putWasmAST WA.OpI32Divs=putOpcode 0x6d
    putWasmAST WA.OpI32Divu=putOpcode 0x6e
    putWasmAST WA.OpI32Rems=putOpcode 0x6f
    putWasmAST WA.OpI32Remu=putOpcode 0x70
    putWasmAST WA.OpI32And=putOpcode 0x71
    putWasmAST WA.OpI32Or=putOpcode 0x72
    putWasmAST WA.OpI32Xor=putOpcode 0x73
    putWasmAST WA.OpI32Shl=putOpcode 0x74
    putWasmAST WA.OpI32Shrs=putOpcode 0x75
    putWasmAST WA.OpI32Shru=putOpcode 0x76
    putWasmAST WA.OpI32Rotl=putOpcode 0x77
    putWasmAST WA.OpI32Rotr=putOpcode 0x78
    putWasmAST WA.OpI64Clz=putOpcode 0x79
    putWasmAST WA.OpI64Ctz=putOpcode 0x7a
    putWasmAST WA.OpI64Popcnt=putOpcode 0x7b
    putWasmAST WA.OpI64Add=putOpcode 0x7c
    putWasmAST WA.OpI64Sub=putOpcode 0x7d
    putWasmAST WA.OpI64Mul=putOpcode 0x7e
    putWasmAST WA.OpI64Divs=putOpcode 0x7f
    putWasmAST WA.OpI64Divu=putOpcode 0x80
    putWasmAST WA.OpI64Rems=putOpcode 0x81
    putWasmAST WA.OpI64Remu=putOpcode 0x82
    putWasmAST WA.OpI64And=putOpcode 0x83
    putWasmAST WA.OpI64Or=putOpcode 0x84
    putWasmAST WA.OpI64Xor=putOpcode 0x85
    putWasmAST WA.OpI64Shl=putOpcode 0x86
    putWasmAST WA.OpI64Shrs=putOpcode 0x87
    putWasmAST WA.OpI64Shru=putOpcode 0x88
    putWasmAST WA.OpI64Rotl=putOpcode 0x89
    putWasmAST WA.OpI64Rotr=putOpcode 0x8a
    putWasmAST WA.OpF32Abs=putOpcode 0x8b
    putWasmAST WA.OpF32Neg=putOpcode 0x8c
    putWasmAST WA.OpF32Ceil=putOpcode 0x8d
    putWasmAST WA.OpF32Floor=putOpcode 0x8e
    putWasmAST WA.OpF32Trunc=putOpcode 0x8f
    putWasmAST WA.OpF32Nearest=putOpcode 0x90
    putWasmAST WA.OpF32Sqrt=putOpcode 0x91
    putWasmAST WA.OpF32Add=putOpcode 0x92
    putWasmAST WA.OpF32Sub=putOpcode 0x93
    putWasmAST WA.OpF32Mul=putOpcode 0x94
    putWasmAST WA.OpF32Div=putOpcode 0x95
    putWasmAST WA.OpF32Min=putOpcode 0x96
    putWasmAST WA.OpF32Max=putOpcode 0x97
    putWasmAST WA.OpF32Copysign=putOpcode 0x98
    putWasmAST WA.OpF64Abs=putOpcode 0x99
    putWasmAST WA.OpF64Neg=putOpcode 0x9a
    putWasmAST WA.OpF64Ceil=putOpcode 0x9b
    putWasmAST WA.OpF64Floor=putOpcode 0x9c
    putWasmAST WA.OpF64Trunc=putOpcode 0x9d
    putWasmAST WA.OpF64Nearest=putOpcode 0x9e
    putWasmAST WA.OpF64Sqrt=putOpcode 0x9f
    putWasmAST WA.OpF64Add=putOpcode 0xa0
    putWasmAST WA.OpF64Sub=putOpcode 0xa1
    putWasmAST WA.OpF64Mul=putOpcode 0xa2
    putWasmAST WA.OpF64Div=putOpcode 0xa3
    putWasmAST WA.OpF64Min=putOpcode 0xa4
    putWasmAST WA.OpF64Max=putOpcode 0xa5
    putWasmAST WA.OpF64Copysign=putOpcode 0xa6
    putWasmAST WA.OpI32WrapI64=putOpcode 0xa7
    putWasmAST WA.OpI32TruncsF32=putOpcode 0xa8
    putWasmAST WA.OpI32TrancuF32=putOpcode 0xa9
    putWasmAST WA.OpI32TrancsF64=putOpcode 0xaa
    putWasmAST WA.OpI32TrancuF64=putOpcode 0xab
    putWasmAST WA.OpI64ExtendsI32=putOpcode 0xac
    putWasmAST WA.OpI64ExtenduI32=putOpcode 0xad
    putWasmAST WA.OpI64TruncsF32=putOpcode 0xae
    putWasmAST WA.OpI64TrancuF32=putOpcode 0xaf
    putWasmAST WA.OpI64TrancsF64=putOpcode 0xb0
    putWasmAST WA.OpI64TrancuF64=putOpcode 0xb1
    putWasmAST WA.OpF32ConvertsI32=putOpcode 0xb2
    putWasmAST WA.OpF32ConvertuI32=putOpcode 0xb3
    putWasmAST WA.OpF32ConvertsI64=putOpcode 0xb4
    putWasmAST WA.OpF32ConvertuI64=putOpcode 0xb5
    putWasmAST WA.OpF32DemoteF64=putOpcode 0xb6
    putWasmAST WA.OpF64ConvertsI32=putOpcode 0xb7
    putWasmAST WA.OpF64ConvertuI32=putOpcode 0xb8
    putWasmAST WA.OpF64ConvertsI64=putOpcode 0xb9
    putWasmAST WA.OpF64ConvertuI64=putOpcode 0xba
    putWasmAST WA.OpF64PromoteF32=putOpcode 0xbb
    putWasmAST WA.OpI32ReinterpretF32=putOpcode 0xbc
    putWasmAST WA.OpI64ReinterpretF64=putOpcode 0xbd
    putWasmAST WA.OpF32ReinterpretI32=putOpcode 0xbe
    putWasmAST WA.OpF64ReinterpretI64=putOpcode 0xbf

instance WasmAST WA.WasmASTRoot where
    putWasmAST ast=do
        putUint32 0x6d736100
        putUint32 0x1
        putMaybe $ ast ^. WA.typeSection
        putMaybe $ ast ^. WA.importSection
        putMaybe $ ast ^. WA.functionSection
        putMaybe $ ast ^. WA.tableSection
        putMaybe $ ast ^. WA.memorySection
        putMaybe $ ast ^. WA.globalSection
        putMaybe $ ast ^. WA.exportSection
        putMaybe $ ast ^. WA.startSection
        putMaybe $ ast ^. WA.elementSection
        putMaybe $ ast ^. WA.codeSection
        putMaybe $ ast ^. WA.dataSection

