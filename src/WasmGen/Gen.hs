module WasmGen.Gen where

import qualified Wasm.AST                      as WA
import qualified Parsers.Expr                  as PE
import qualified Data.Map                      as M
import qualified Parsers.Lang                  as L

type MemberMap=(M.Map String Int,M.Map String [(L.Ident,L.Type)])

sizeOf :: L.Type -> Int
sizeOf L.TI32  = 4
sizeOf L.TI64  = 8
sizeOf L.TF32  = 4
sizeOf L.TF64  = 8
sizeOf L.TBool = 4
sizeOf L.TChar = 4
sizeOf _       = 4

