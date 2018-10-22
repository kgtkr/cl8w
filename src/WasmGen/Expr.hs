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
import qualified WasmGen.Lang                  as WL
import qualified WasmGen.Member                as WM

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
    _functionMap:: WM.FunctionMap,
    _structMap:: WM.StructMap,
    _exprType:: ExprType
}
makeLenses ''ExprGenData

type ExprGen = State ExprGenData

addLocal :: W.ValueType -> ExprGen Int
addLocal t = do
    len <- use localsLen
    localsLen += 1
    locals %= (flip D.snoc) t
    return len

addNamedLocalData :: L.Type -> L.Ident -> ExprGen Int
addNamedLocalData t name = do
    id <- (addLocal . WL.typeToValueType) t
    localsMap %= (M.insert name (t, id))
    return id

addOpCode :: W.OperatorCode -> ExprGen ()
addOpCode x = opCodes %= (flip D.snoc) x

callGen :: WM.FunctionMap -> String -> [ExprGen ()] -> ExprGen ()
callGen m f args = do
    let (id, Me.FuncDef _ _ re) = m M.! f
    opCallGen (W.OpCall id) args
    exprType .= re

opCallGen :: W.OperatorCode -> [ExprGen ()] -> ExprGen ()
opCallGen op args = do
    sequence_ args
    addOpCode op

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
        res  <- addLocal W.ValI32
        callGen fMap "malloc" [addOpCode $ W.OpI32Const (WM.structSize sDef)]
        mapM_
            (\(ident, ex) -> do
                let prop = sDef M.! ident
                let storeOp =
                        (case WL.typeToValueType (WM._typ prop) of
                                W.ValI32 -> W.OpI32Store
                                W.ValI64 -> W.OpI64Store
                                W.ValF32 -> W.OpF32Store
                                W.ValF64 -> W.OpF64Store
                            )
                            (W.MemoryImmediate 2 (WM._pos prop))
                opCallGen storeOp [addOpCode $ W.OpGetLocal res, exprGen ex]
            )
            exprs
        exprType .= (Just . L.RefType . L.TStruct) name
        addOpCode $ W.OpGetLocal res
    E.EI32L x -> addOpCode $ W.OpI32Const x
    E.EI64L x -> addOpCode $ W.OpI64Const x
    E.EF32L x -> addOpCode $ W.OpF32Const x
    E.EF64L x -> addOpCode $ W.OpF64Const x
