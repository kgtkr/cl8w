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


instance WasmAST OperatorCode where
    putWasmAST=undefined
