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
