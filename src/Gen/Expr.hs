module Gen.Expr where

import qualified Data.DList                    as D
import qualified Wasm.AST                      as WA
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Parsers.Member                as PM
import qualified Parsers.Lang                  as PL
import qualified Parsers.Expr                  as PE
import           Control.Lens
import qualified Gen.Lang                      as GL
import           Control.Monad.Reader
import qualified Gen.FuncGen                 as FG
type ExprType=Maybe PL.Type

opStore :: WA.ValueType -> WA.MemoryImmediate -> WA.OperatorCode
opStore t =
    (case t of
        WA.ValI32 -> WA.OpI32Store
        WA.ValI64 -> WA.OpI64Store
        WA.ValF32 -> WA.OpF32Store
        WA.ValF64 -> WA.OpF64Store
    )

opLoad :: WA.ValueType -> WA.MemoryImmediate -> WA.OperatorCode
opLoad t =
    (case t of
        WA.ValI32 -> WA.OpI32Load
        WA.ValI64 -> WA.OpI64Load
        WA.ValF32 -> WA.OpF32Load
        WA.ValF64 -> WA.OpF64Load
    )

exprType :: PE.Expr -> FG.FuncGen (Maybe PL.Type)
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
exprType (PE.EVar   ident ) = Just . (^. _1) . (M.! ident) <$> use FG.symbolMap
exprType (PE.ECall _ ident) = do
    Just (PL.RefType (PL.TFunc _ res)) <- exprType ident
    return res
exprType (PE.ENot   _        ) = (return . Just) PL.TBool
exprType (PE.EPlus  e        ) = exprType e
exprType (PE.EMinus e        ) = exprType e
exprType (PE.EMember pIdent e) = do
    Just (PL.RefType (PL.TStruct sIdent)) <- exprType e
    Just . (^. GL.typ) . (M.! pIdent) . (M.! sIdent) <$> view GL.structs
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
exprType (PE.EBlock  _ e) = case e of
    Just e  -> exprType e
    Nothing -> return Nothing
exprType (PE.ELet _ _      ) = return Nothing
exprType (PE.EIf (_, e) _ _) = exprType e
exprType (PE.EWhile _ _    ) = return Nothing
exprType (PE.EReturn _     ) = return Nothing
exprType (PE.ESet _ _      ) = return Nothing
exprType (PE.EFor _ _ _ _  ) = return Nothing

makeScope :: FG.FuncGen () -> FG.FuncGen ()
makeScope m = do
    lm <- use FG.symbolMap
    m
    FG.symbolMap .= lm
    return ()

initGen :: FG.FuncGen ()
initGen = do
    params <- use FG.params
    ( mapM_
                (\(i, (ident, t)) -> addNamedLocalData t ident
                    >> setNamedLocal ident (addOpCode $ WA.OpGetLocal i)
                )
        . zip [0 ..]
        )
        params

callGen :: String -> [FG.FuncGen ()] -> FG.FuncGen ()
callGen f args = do
    m <- view GL.functions
    let (id, _) = m M.! f
    opCallGen (WA.OpCall id) args

addLocal :: WA.ValueType -> FG.FuncGen Int
addLocal t = do
    len <- use FG.localsLen
    FG.localsLen += 1
    FG.locals %= (`D.snoc` t)
    return len

addNamedLocalData :: PL.Type -> PL.Ident -> FG.FuncGen ()
addNamedLocalData t name = do
    id <- addLocal WA.ValI32
    FG.symbolMap %= M.insert name (t, FG.SDLocal id)
    mallocGen $ addOpCode $ WA.OpI32Const $ GL.sizeOf t
    addOpCode $ WA.OpSetLocal id

setNamedLocal :: PL.Ident -> FG.FuncGen () -> FG.FuncGen ()
setNamedLocal ident e = do
    (t, FG.SDLocal id) <- (M.! ident) <$> use FG.symbolMap
    addOpCode $ WA.OpGetLocal id
    e
    addOpCode $ opStore (GL.typeToValueType t) (WA.MemoryImmediate 0)

getNamedLocal :: PL.Ident -> FG.FuncGen ()
getNamedLocal ident = do
    (t, FG.SDLocal id) <- (M.! ident) <$> use FG.symbolMap
    addOpCode $ WA.OpGetLocal id
    addOpCode $ opLoad (GL.typeToValueType t) (WA.MemoryImmediate 0)
    return ()

addOpCode :: WA.OperatorCode -> FG.FuncGen ()
addOpCode x = FG.opCodes %= (`D.snoc` x)

opCallGen :: WA.OperatorCode -> [FG.FuncGen ()] -> FG.FuncGen ()
opCallGen op args = do
    sequence_ args
    addOpCode op

-- 値/位置/オフセット
storeGen :: PE.Expr -> PE.Expr -> Int -> FG.FuncGen ()
storeGen e i o = do
    Just et <- exprType e
    let storeOp = opStore (GL.typeToValueType et) (WA.MemoryImmediate o)
    opCallGen storeOp [exprGen i, exprGen e]
dropExprGen :: PE.Expr -> FG.FuncGen ()
dropExprGen e = do
    t <- exprType e
    case t of
        Just _ -> do
            exprGen e
            addOpCode WA.OpDrop
        Nothing -> exprGen e

mallocGen :: FG.FuncGen () -> FG.FuncGen ()
mallocGen m = callGen "malloc" [m]

exprGen :: PE.Expr -> FG.FuncGen ()
exprGen (PE.EStructL name exprs) = do
    sDef <- (M.! name) <$> view GL.structs
    res  <- addLocal WA.ValI32
    mallocGen (addOpCode $ WA.OpI32Const (GL.structSize sDef))
    addOpCode $ WA.OpSetLocal res
    mapM_
        (\(ident, ex) -> do
            let prop = sDef M.! ident
            let storeOp = opStore (GL.typeToValueType (prop ^. GL.typ))
                                  (WA.MemoryImmediate (prop ^. GL.pos))
            opCallGen storeOp [addOpCode $ WA.OpGetLocal res, exprGen ex]
        )
        exprs
    addOpCode $ WA.OpGetLocal res
exprGen (PE.EI32L x    ) = addOpCode $ WA.OpI32Const x
exprGen (PE.EI64L x    ) = addOpCode $ WA.OpI64Const x
exprGen (PE.EF32L x    ) = addOpCode $ WA.OpF32Const x
exprGen (PE.EF64L x    ) = addOpCode $ WA.OpF64Const x
exprGen (PE.EArrayL t x) = do
    let size = GL.sizeOf t
    mallocGen $ sequence_
        [addOpCode $ WA.OpI32Const size, exprGen x, addOpCode WA.OpI32Mul]
exprGen (PE.EBoolL x) = addOpCode $ WA.OpI32Const (if x then 1 else 0)
exprGen (PE.EVar   x) = do
    l <- snd . (M.! x) <$> use FG.symbolMap
    case l of
        FG.SDLocal _ -> getNamedLocal x
        FG.SDFunc  x -> addOpCode $ WA.OpI32Const x
exprGen (PE.ECall ex f) = do
    mapM_ exprGen ex

    sMap <- use FG.symbolMap
    case (fmap (\x -> snd (sMap M.! x)) . getIdent) f >>= getFuncID of
        -- 関数を静的呼び出し出来る時は最適化
        Just x  -> addOpCode $ WA.OpCall x
        Nothing -> do
            types <- view GL.types
            Just (PL.RefType (PL.TFunc params res)) <- exprType f
            let waType = WA.FuncType (fmap GL.typeToValueType params)
                                     (fmap GL.typeToValueType res)
            let typeID = types M.! waType
            exprGen f
            addOpCode $ WA.OpCallIndirect typeID
  where
    getIdent (PE.EVar x) = Just x
    getIdent _           = Nothing
    getFuncID (FG.SDFunc x) = Just x
    getFuncID _             = Nothing
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
exprGen (PE.EMember ident e) = do
    Just (PL.RefType (PL.TStruct sName)) <- exprType e
    prop <- (M.! ident) . (M.! sName) <$> view GL.structs
    exprGen e
    let loadOp = opLoad (GL.typeToValueType (prop ^. GL.typ))
                        (WA.MemoryImmediate (prop ^. GL.pos))
    addOpCode loadOp

    return ()
exprGen (PE.EIndex index e) = do
    Just (PL.RefType (PL.TArray t)) <- exprType e
    let size = GL.sizeOf t
    exprGen e
    addOpCode $ WA.OpI32Const size
    exprGen index
    addOpCode WA.OpI32Mul
    addOpCode WA.OpI32Add
    let loadOp = opLoad (GL.typeToValueType t) (WA.MemoryImmediate 0)
    addOpCode loadOp
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
exprGen (PE.EBlock ss e) = makeScope $ do
    mapM_ dropExprGen ss
    case e of
        Just e  -> exprGen e
        Nothing -> return ()
exprGen (PE.ELet name e) = do
    Just t <- exprType e
    x      <- addNamedLocalData t name
    setNamedLocal name (exprGen e)
exprGen (PE.EIf (e, s1) [] s2) = do
    t <- exprType s1
    exprGen e
    addOpCode $ WA.OpIf $ WA.BlockType (GL.typeToValueType <$> t)
    exprGen s1
    case s2 of
        Just s2 -> do
            addOpCode WA.OpElse
            exprGen s2
        Nothing -> return ()
    addOpCode $ WA.OpEnd
exprGen (PE.EWhile a b) = do
    addOpCode $ WA.OpLoop (WA.BlockType Nothing)
    exprGen a
    addOpCode $ WA.OpIf $ WA.BlockType Nothing
    exprGen b
    addOpCode $ WA.OpBr 1
    addOpCode $ WA.OpEnd
    addOpCode $ WA.OpEnd
    return ()
exprGen (PE.EReturn e) = do
    forM_ e exprGen
    addOpCode $ WA.OpReturn
exprGen (PE.ESet a b) = do
    case a of
        PE.EVar ident -> do
            setNamedLocal ident (exprGen b)
            return ()
        PE.EIndex i e -> do
            Just (PL.RefType (PL.TArray t)) <- exprType e
            let size = GL.sizeOf t
            exprGen e
            addOpCode $ WA.OpI32Const size
            exprGen i
            addOpCode WA.OpI32Mul
            addOpCode WA.OpI32Add
            let storeOp = opStore (GL.typeToValueType t) (WA.MemoryImmediate 0)
            exprGen b
            addOpCode storeOp
            return ()
        PE.EMember ident e -> do
            Just (PL.RefType (PL.TStruct sName)) <- exprType e
            prop <- (M.! ident) . (M.! sName) <$> view GL.structs
            exprGen e
            let storeOp = opStore (GL.typeToValueType (prop ^. GL.typ))
                                  (WA.MemoryImmediate (prop ^. GL.pos))
            exprGen b
            addOpCode storeOp
            return ()
    return ()
exprGen (PE.EFor init cond proc body) = makeScope $ do
    exprGen init
    addOpCode $ WA.OpLoop (WA.BlockType Nothing)
    exprGen cond
    addOpCode $ WA.OpIf $ WA.BlockType Nothing
    exprGen body
    exprGen proc
    addOpCode $ WA.OpBr 1
    addOpCode $ WA.OpEnd
    addOpCode $ WA.OpEnd
    return ()
