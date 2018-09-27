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

putArray :: (WasmAST a) => [a] -> Put
putArray x = do
    putVaruint32 $ length x
    mapM_ putWasmAST x

putBytes :: Putter BS.ByteString
putBytes x = do
    putVaruint32 $ BS.length x
    putByteString x

putString :: Putter String
putString = (putBytes . BSU.fromString)

class WasmAST a where
    putWasmAST::Putter a

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
        putArray p
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
