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
import           Control.Monad.Reader
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
    _localsMap:: LocalsMap
}
makeLenses ''ExprGenData

type ExprGen = ReaderT WM.MemberData (State ExprGenData)

exprType :: E.Expr -> ExprGen (Maybe L.Type)
exprType (E.EStructL ident _) = (return . Just) $ L.RefType $ L.TStruct ident
exprType (E.EI32L    _      ) = (return . Just) L.TI32
exprType (E.EI64L    _      ) = (return . Just) L.TI64
exprType (E.EF32L    _      ) = (return . Just) L.TF32
exprType (E.EF64L    _      ) = (return . Just) L.TF64
exprType (E.EStringL _      ) = (return . Just) $ L.RefType L.TString
exprType (E.EArrayL t _     ) = (return . Just) $ L.RefType $ L.TArray t
exprType (E.EBoolL _        ) = (return . Just) L.TBool
exprType (E.ECharL _        ) = (return . Just) L.TChar
exprType (E.EVar   ident    ) = Just . (^. _1) . (M.! ident) <$> use localsMap
exprType (E.ECall ident _   ) = do
    Me.FuncDef _ _ t <- (^. _2) . (M.! ident) <$> view WM.functions
    return t
exprType (E.ENot   _        ) = (return . Just) L.TBool
exprType (E.EPlus  e        ) = exprType e
exprType (E.EMinus e        ) = exprType e
exprType (E.EMember pIdent e) = do
    Just (L.RefType (L.TStruct sIdent)) <- exprType e
    Just . (^. WM.typ) . (M.! pIdent) . (M.! sIdent) <$> view WM.structs
exprType (E.EIndex _ e) = do
    Just (L.RefType (L.TArray t)) <- exprType e
    (return . Just) t
exprType (E.EAdd    e _) = exprType e
exprType (E.ESub    e _) = exprType e
exprType (E.EMul    e _) = exprType e
exprType (E.EDiv    e _) = exprType e
exprType (E.EMod    e _) = exprType e
exprType (E.EAnd    _ _) = (return . Just) L.TBool
exprType (E.EOr     _ _) = (return . Just) L.TBool
exprType (E.EBitAnd e _) = exprType e
exprType (E.EBitOr  e _) = exprType e
exprType (E.EPow    e _) = exprType e
exprType (E.EEq     _ _) = (return . Just) L.TBool
exprType (E.ENe     _ _) = (return . Just) L.TBool
exprType (E.ELt     _ _) = (return . Just) L.TBool
exprType (E.ELte    _ _) = (return . Just) L.TBool
exprType (E.EGt     _ _) = (return . Just) L.TBool
exprType (E.EGte    _ _) = (return . Just) L.TBool

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

-- 値/位置/オフセット
storeGen :: E.Expr -> E.Expr -> Int -> ExprGen ()
storeGen e i o = do
    Just et <- exprType e
    let storeOp =
            (case WL.typeToValueType et of
                    W.ValI32 -> W.OpI32Store
                    W.ValI64 -> W.OpI64Store
                    W.ValF32 -> W.OpF32Store
                    W.ValF64 -> W.OpF64Store
                )
                (W.MemoryImmediate 2 o)
    opCallGen storeOp [exprGen i, exprGen e]

exprGen :: E.Expr -> ExprGen ()
exprGen expr = case expr of
    E.EStructL name exprs -> blockGen (W.BlockType (Just W.ValI32)) $ do
        fMap <- view WM.functions
        sDef <- (M.! name) <$> view WM.structs
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
        addOpCode $ W.OpGetLocal res
    E.EI32L  x -> addOpCode $ W.OpI32Const x
    E.EI64L  x -> addOpCode $ W.OpI64Const x
    E.EF32L  x -> addOpCode $ W.OpF32Const x
    E.EF64L  x -> addOpCode $ W.OpF64Const x
    E.EBoolL x -> addOpCode $ W.OpI32Const (if x then 1 else 0)
    E.EVar   x -> do
        l <- snd . (M.! x) <$> use localsMap
        addOpCode $ W.OpGetLocal l
    E.ECall name ex -> do
        fMap <- view WM.functions
        callGen fMap name (fmap exprGen ex)
    E.ENot x -> do
        exprGen x
        addOpCode $ W.OpI32Const 0
        addOpCode W.OpI32Eq
    E.EPlus  x -> exprGen x
    E.EMinus x -> do
        t <- exprType x
        case t of
            Just L.TI32 -> do
                exprGen x
                addOpCode $ W.OpI32Const (-1)
                addOpCode $ W.OpI32Mul
    E.EAdd a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Add
    E.ESub a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Sub
    E.EMul a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Mul
    E.EDiv a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Divs
    E.EMod a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Rems
    E.EEq a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Eq
    E.ENe a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Ne
    E.EGt a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Gts
    E.EGte a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Ges
    E.ELt a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Lts
    E.ELte a b -> do
        ta <- exprType a
        tb <- exprType b
        case (ta, tb) of
            (Just L.TI32, Just L.TI32) -> do
                exprGen a
                exprGen b
                addOpCode $ W.OpI32Les

