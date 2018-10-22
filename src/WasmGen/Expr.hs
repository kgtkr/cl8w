{-# LANGUAGE TemplateHaskell #-}


module WasmGen.Expr where

import qualified Data.DList                    as D
import qualified Wasm.AST                      as W
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Parsers.Member                as Me
import qualified Parsers.Lang                  as L
import qualified Parsers.Expr                  as E
import           Control.Lens

type FunctionMap=M.Map String (Int,Me.FuncDef)
type StructMap=M.Map String Me.StructMembers
type OpCodes=D.DList W.OperatorCode
type Locals=D.DList W.ValueType
type LocalsLen=Int
type LocalsMap=M.Map String LocalData
type LocalData=(L.Type,Int)

type ExprType=Maybe L.Type

data ExprGenData=ExprGenData{
    _opCodes::OpCodes,
    _locals:: Locals,
    _localsLen:: LocalsLen,
    _localsMap:: LocalsMap,
    _functionMap:: FunctionMap,
    _structMap:: StructMap,
    _exprType:: ExprType
}
makeLenses ''ExprGenData

type ExprGen = State ExprGenData

addLocal :: L.Type -> ExprGen Int
addLocal t = do
    len <- use localsLen
    localsLen += 1
    locals %= (flip D.snoc . typeToValueType) t
    return len

addNamedLocalData :: L.Type -> L.Ident -> ExprGen Int
addNamedLocalData t name = do
    id <- addLocal t
    localsMap %= (M.insert name (t, id))
    return id

addOpCode :: W.OperatorCode -> ExprGen ()
addOpCode x = opCodes %= (flip D.snoc) x

callGen :: FunctionMap -> String -> [ExprGen ()] -> ExprGen ()
callGen m f args = do
    sequence_ args
    let (id, Me.FuncDef _ _ re) = m M.! f
    addOpCode $ W.OpCall id
    exprType .= re

blockGen :: W.BlockType -> ExprGen () -> ExprGen ()
blockGen t x = do
    addOpCode $ W.OpBlock t
    x
    addOpCode $ W.OpEnd

mapSort :: Ord a => [a] -> M.Map a b -> [b]
mapSort keys m = map (m M.!) keys

exprGen :: E.Expr -> ExprGen ()
exprGen expr = case expr of
    E.EStructL name exprs -> blockGen (W.BlockType (Just W.ValI32)) $ do
        fMap <- use functionMap
        sDef <- (M.! name) <$> use structMap
        callGen fMap "malloc" [addOpCode $ W.OpI32Const (structSize sDef)]
        -- TODO 式の値を書き込む
        exprType .= (Just . L.RefType . L.TStruct) name
    E.EI32L x -> addOpCode $ W.OpI32Const x
    E.EI64L x -> addOpCode $ W.OpI64Const x
    E.EF32L x -> addOpCode $ W.OpF32Const x
    E.EF64L x -> addOpCode $ W.OpF64Const x

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
