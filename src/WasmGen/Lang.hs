module WasmGen.Lang where

import qualified Parsers.Lang                  as L
import qualified Wasm.AST                      as W
import qualified Parsers.Member                as Me

typeToValueType :: L.Type -> W.ValueType
typeToValueType L.TI32        = W.ValI32
typeToValueType L.TI64        = W.ValI64
typeToValueType L.TF32        = W.ValF32
typeToValueType L.TF64        = W.ValF64
typeToValueType L.TBool       = W.ValI32
typeToValueType L.TChar       = W.ValI32
typeToValueType (L.RefType _) = W.ValI32

sizeOf :: L.Type -> Int
sizeOf L.TI32        = 4
sizeOf L.TI64        = 8
sizeOf L.TF32        = 4
sizeOf L.TF64        = 8
sizeOf L.TBool       = 4
sizeOf L.TChar       = 4
sizeOf (L.RefType _) = 4

structSize :: Me.StructMembers -> Int
structSize = sum . map (sizeOf . snd)
