{-# LANGUAGE TemplateHaskell #-}


module Gen.Expr where

import qualified Data.DList                    as D
import qualified Wasm.AST                      as WA
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Parsers.Member                as PM
import qualified Parsers.Lang                  as PL
import qualified Parsers.Expr                  as PE
import           Control.Lens
import qualified Gen.Lang                      as WL
import           Control.Monad.Reader
import qualified Gen.OpCodeGen                 as GO


type ExprType=Maybe PL.Type

exprType :: PE.Expr -> GO.OpCodeGen (Maybe PL.Type)
exprType (PE.EStructL ident _) =
    (return . Just) $ PL.RefType $ PL.TStruct ident
exprType (PE.EI32L    _   ) = (return . Just) PL.TI32
exprType (PE.EI64L    _   ) = (return . Just) PL.TI64
exprType (PE.EF32L    _   ) = (return . Just) PL.TF32
exprType (PE.EF64L    _   ) = (return . Just) PL.TF64
exprType (PE.EStringL _   ) = (return . Just) $ PL.RefType PL.TString
exprType (PE.EArrayL t _  ) = (return . Just) $ PL.RefType $ PL.TArray t
exprType (PE.EBoolL _     ) = (return . Just) PL.TBool
exprType (PE.ECharL _     ) = (return . Just) PL.TChar
exprType (PE.EVar   ident ) = Just . (^. _1) . (M.! ident) <$> use GO.localsMap
exprType (PE.ECall ident _) = do
    PM.FuncDef _ _ t <- (^. _2) . (M.! ident) <$> view WL.functions
    return t
exprType (PE.ENot   _        ) = (return . Just) PL.TBool
exprType (PE.EPlus  e        ) = exprType e
exprType (PE.EMinus e        ) = exprType e
exprType (PE.EMember pIdent e) = do
    Just (PL.RefType (PL.TStruct sIdent)) <- exprType e
    Just . (^. WL.typ) . (M.! pIdent) . (M.! sIdent) <$> view WL.structs
exprType (PE.EIndex _ e) = do
    Just (PL.RefType (PL.TArray t)) <- exprType e
    (return . Just) t
exprType (PE.EAdd    e _) = exprType e
exprType (PE.ESub    e _) = exprType e
exprType (PE.EMul    e _) = exprType e
exprType (PE.EDiv    e _) = exprType e
exprType (PE.EMod    e _) = exprType e
exprType (PE.EAnd    _ _) = (return . Just) PL.TBool
exprType (PE.EOr     _ _) = (return . Just) PL.TBool
exprType (PE.EBitAnd e _) = exprType e
exprType (PE.EBitOr  e _) = exprType e
exprType (PE.EPow    e _) = exprType e
exprType (PE.EEq     _ _) = (return . Just) PL.TBool
exprType (PE.ENe     _ _) = (return . Just) PL.TBool
exprType (PE.ELt     _ _) = (return . Just) PL.TBool
exprType (PE.ELte    _ _) = (return . Just) PL.TBool
exprType (PE.EGt     _ _) = (return . Just) PL.TBool
exprType (PE.EGte    _ _) = (return . Just) PL.TBool

addLocal :: WA.ValueType -> GO.OpCodeGen Int
addLocal t = do
    len <- use GO.localsLen
    GO.localsLen += 1
    GO.locals %= (flip D.snoc) t
    return len

addNamedLocalData :: PL.Type -> PL.Ident -> GO.OpCodeGen Int
addNamedLocalData t name = do
    id <- (addLocal . WL.typeToValueType) t
    GO.localsMap %= (M.insert name (t, id))
    return id

addOpCode :: WA.OperatorCode -> GO.OpCodeGen ()
addOpCode x = GO.opCodes %= (flip D.snoc) x

callGen :: WL.FunctionMap -> String -> [GO.OpCodeGen ()] -> GO.OpCodeGen ()
callGen m f args = do
    let (id, PM.FuncDef _ _ re) = m M.! f
    opCallGen (WA.OpCall id) args

opCallGen :: WA.OperatorCode -> [GO.OpCodeGen ()] -> GO.OpCodeGen ()
opCallGen op args = do
    sequence_ args
    addOpCode op

blockGen :: WA.BlockType -> GO.OpCodeGen () -> GO.OpCodeGen ()
blockGen t x = do
    addOpCode $ WA.OpBlock t
    x
    addOpCode $ WA.OpEnd

mapSort :: Ord a => [a] -> M.Map a b -> [b]
mapSort keys m = map (m M.!) keys

-- 値/位置/オフセット
storeGen :: PE.Expr -> PE.Expr -> Int -> GO.OpCodeGen ()
storeGen e i o = do
    Just et <- exprType e
    let storeOp =
            (case WL.typeToValueType et of
                    WA.ValI32 -> WA.OpI32Store
                    WA.ValI64 -> WA.OpI64Store
                    WA.ValF32 -> WA.OpF32Store
                    WA.ValF64 -> WA.OpF64Store
                )
                (WA.MemoryImmediate 2 o)
    opCallGen storeOp [exprGen i, exprGen e]

exprGen :: PE.Expr -> GO.OpCodeGen ()
exprGen (PE.EStructL name exprs) =
    blockGen (WA.BlockType (Just WA.ValI32)) $ do
        fMap <- view WL.functions
        sDef <- (M.! name) <$> view WL.structs
        res  <- addLocal WA.ValI32
        callGen fMap "malloc" [addOpCode $ WA.OpI32Const (WL.structSize sDef)]
        mapM_
            (\(ident, ex) -> do
                let prop = sDef M.! ident
                let storeOp =
                        (case WL.typeToValueType (WL._typ prop) of
                                WA.ValI32 -> WA.OpI32Store
                                WA.ValI64 -> WA.OpI64Store
                                WA.ValF32 -> WA.OpF32Store
                                WA.ValF64 -> WA.OpF64Store
                            )
                            (WA.MemoryImmediate 2 (WL._pos prop))
                opCallGen storeOp [addOpCode $ WA.OpGetLocal res, exprGen ex]
            )
            exprs
        addOpCode $ WA.OpGetLocal res
exprGen (PE.EI32L  x) = addOpCode $ WA.OpI32Const x
exprGen (PE.EI64L  x) = addOpCode $ WA.OpI64Const x
exprGen (PE.EF32L  x) = addOpCode $ WA.OpF32Const x
exprGen (PE.EF64L  x) = addOpCode $ WA.OpF64Const x
exprGen (PE.EBoolL x) = addOpCode $ WA.OpI32Const (if x then 1 else 0)
exprGen (PE.EVar   x) = do
    l <- snd . (M.! x) <$> use GO.localsMap
    addOpCode $ WA.OpGetLocal l
exprGen (PE.ECall name ex) = do
    fMap <- view WL.functions
    callGen fMap name (fmap exprGen ex)
exprGen (PE.ENot x) = do
    exprGen x
    addOpCode $ WA.OpI32Const 0
    addOpCode WA.OpI32Eq
exprGen (PE.EPlus  x) = exprGen x
exprGen (PE.EMinus x) = do
    t <- exprType x
    case t of
        Just PL.TI32 -> do
            exprGen x
            addOpCode $ WA.OpI32Const (-1)
            addOpCode $ WA.OpI32Mul
exprGen (PE.EAdd a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Add
exprGen (PE.ESub a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Sub
exprGen (PE.EMul a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Mul
exprGen (PE.EDiv a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Divs
exprGen (PE.EMod a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Rems
exprGen (PE.EEq a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Eq
exprGen (PE.ENe a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Ne
exprGen (PE.EGt a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Gts
exprGen (PE.EGte a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Ges
exprGen (PE.ELt a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Lts
exprGen (PE.ELte a b) = do
    ta <- exprType a
    tb <- exprType b
    case (ta, tb) of
        (Just PL.TI32, Just PL.TI32) -> do
            exprGen a
            exprGen b
            addOpCode $ WA.OpI32Les

