module WasmGen.Gen where

import qualified Wasm.AST                      as WA
import qualified Parsers.Expr                  as PE
import qualified Data.Map                      as M
import qualified Parsers.Lang                  as L

import qualified Parsers.Member                as Me
import           Data.Tuple                     ( swap )
import           Data.Maybe                     ( mapMaybe )

type MemberMap=(M.Map String Int,M.Map String Me.StructMembers)

sizeOf :: L.Type -> Int
sizeOf L.TI32  = 4
sizeOf L.TI64  = 8
sizeOf L.TF32  = 4
sizeOf L.TF64  = 8
sizeOf L.TBool = 4
sizeOf L.TChar = 4
sizeOf _       = 4

memberMap :: [Me.Member] -> MemberMap
memberMap m =
    ( (M.fromList . map swap . zip [0 ..] . mapMaybe fnMap) m
    , (M.fromList . mapMaybe stMap) m
    )
  where
    fnMap (Me.MFun (Me.FuncDef name _ _) _) = Just name
    fnMap (Me.MExternFun (Me.FuncDef name _ _) _) = Just name
    fnMap _ = Nothing
    stMap (Me.MStruct a b) = Just (a, b)
    stMap _                = Nothing
