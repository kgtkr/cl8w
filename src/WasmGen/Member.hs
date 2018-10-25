{-# LANGUAGE TemplateHaskell #-}

module WasmGen.Member where

import qualified Parsers.Lang                  as L
import           Control.Lens
import qualified Data.Map                      as M
import qualified Parsers.Member                as Me
import           WasmGen.Lang
import           Data.List
import qualified WasmGen.Lang                  as WL

import qualified Wasm.AST                      as WA

import qualified Data.DList                    as D

data StructProps=StructProps{
    _pos::Int,
    _typ::L.Type,
    _name::String
}

makeLenses ''StructProps

type Struct=M.Map String StructProps

fromASTStruct :: Me.StructMembers -> Struct
fromASTStruct ms = M.fromList (f 0 (sortOn fst ms))
  where
    f pos ((ident, t) : xs) =
        (ident, StructProps pos t ident) : (f (pos + WL.sizeOf t) xs)
    f _ [] = []

type FunctionMap=M.Map String (Int,Me.FuncDef)
type StructMap=M.Map String Struct

data MemberData=MemberData{
    _functions::FunctionMap,
    _structs::StructMap
}

makeLenses ''MemberData

structSize :: Struct -> Int
structSize = sum . map (WL.sizeOf . _typ . snd) . M.toList

data MemberGen=MemberGen{
    _typeSections::D.DList WA.TypeSection,
    _importSections::D.DList WA.ImportSection,
    _functionSections::D.DList WA.FunctionSection,
    _exportSections::D.DList WA.ExportSection,
    _codeSections::D.DList WA.CodeSection
}
makeLenses ''MemberGen
