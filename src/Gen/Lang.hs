module Gen.Lang where

import qualified Parsers.Lang                  as PL
import qualified Wasm.AST                      as WA
import qualified Parsers.Member                as PM
import           Control.Lens
import qualified Data.Map                      as M

typeToValueType :: PL.Type -> WA.ValueType
typeToValueType PL.TI32        = WA.ValI32
typeToValueType PL.TI64        = WA.ValI64
typeToValueType PL.TF32        = WA.ValF32
typeToValueType PL.TF64        = WA.ValF64
typeToValueType PL.TBool       = WA.ValI32
typeToValueType PL.TChar       = WA.ValI32
typeToValueType (PL.RefType _) = WA.ValI32

sizeOf :: PL.Type -> Int
sizeOf PL.TI32        = 4
sizeOf PL.TI64        = 8
sizeOf PL.TF32        = 4
sizeOf PL.TF64        = 8
sizeOf PL.TBool       = 4
sizeOf PL.TChar       = 4
sizeOf (PL.RefType _) = 4

data StructProps=StructProps{
    _structPropsPos::Int,
    _structPropsTyp::PL.Type,
    _structPropsName::String
}

makeFields ''StructProps

type Struct=M.Map String StructProps

type FunctionMap=M.Map String (Int,PM.FuncDef)
type StructMap=M.Map String Struct

data MemberData=MemberData{
    _memberDataFunctions::FunctionMap,
    _memberDataStructs::StructMap
}

makeFields ''MemberData

structSize :: Struct -> Int
structSize = sum . map (sizeOf . (^. typ) . snd) . M.toList
