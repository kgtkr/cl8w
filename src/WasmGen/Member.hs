{-# LANGUAGE TemplateHaskell #-}

module WasmGen.Member where

import qualified Parsers.Lang                  as L
import           Control.Lens
import qualified Data.Map                      as M
import qualified Parsers.Member                as Me
import           WasmGen.Lang

type FunctionMap=M.Map String (Int,Me.FuncDef)
type StructMap=M.Map String Me.StructMembers

data MemberData=MemberData{
    _functions::FunctionMap,
    _structs::StructMap
}

makeLenses ''MemberData


data StructProps=StructProps{
    _pos::Int,
    _typ::L.Type,
    _name::String
}

makeLenses ''StructProps

type Struct=M.Map String StructProps
