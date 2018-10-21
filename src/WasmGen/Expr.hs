module WasmGen.Expr where

import qualified Data.DList                    as D
import qualified Wasm.AST                      as W
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Parsers.Member                as Me
import qualified Parsers.Lang                  as L
import qualified Parsers.Expr                  as E

data ExprGenData=ExprGenData OpCodes Locals LocalsLen LocalsMap FunctionMap StructMap L.Type
type FunctionMap=M.Map String Int
type StructMap=M.Map String Me.StructMembers
type OpCodes=D.DList W.OperatorCode
type Locals=D.DList W.ValueType
type LocalsLen=Int
type LocalsMap=M.Map String LocalData
type LocalData=(L.Type,Int)
type ExprGen = State ExprGenData

getFunctions :: ExprGen FunctionMap
getFunctions = do
    (ExprGenData _ _ _ _ x _ _) <- get
    return x

getStructs :: ExprGen StructMap
getStructs = do
    (ExprGenData _ _ _ _ _ x _) <- get
    return x

getOpCodes :: ExprGen OpCodes
getOpCodes = do
    (ExprGenData x _ _ _ _ _ _) <- get
    return x

putOpCpdes :: OpCodes -> ExprGen ()
putOpCpdes x = do
    (ExprGenData _ a b c d e f) <- get
    put $ ExprGenData x a b c d e f

modifyOpCodes :: (OpCodes -> OpCodes) -> ExprGen ()
modifyOpCodes f = do
    x <- getOpCodes
    putOpCpdes $ f x

getLocals :: ExprGen Locals
getLocals = do
    (ExprGenData _ x _ _ _ _ _) <- get
    return x

putLocals :: Locals -> ExprGen ()
putLocals x = do
    (ExprGenData a _ b c d e f) <- get
    put $ ExprGenData a x b c d e f

modifyLocals :: (Locals -> Locals) -> ExprGen ()
modifyLocals f = do
    x <- getLocals
    putLocals $ f x

getLocalsLen :: ExprGen LocalsLen
getLocalsLen = do
    (ExprGenData _ _ x _ _ _ _) <- get
    return x

putLocalsLen :: LocalsLen -> ExprGen ()
putLocalsLen x = do
    (ExprGenData a b _ c d e f) <- get
    put $ ExprGenData a b x c d e f

modifyLocalsLen :: (LocalsLen -> LocalsLen) -> ExprGen ()
modifyLocalsLen f = do
    x <- getLocalsLen
    putLocalsLen $ f x

getLocalsMap :: ExprGen LocalsMap
getLocalsMap = do
    (ExprGenData _ _ _ x _ _ _) <- get
    return x

putLocalsMap :: LocalsMap -> ExprGen ()
putLocalsMap x = do
    (ExprGenData a b c _ d e f) <- get
    put $ ExprGenData a b c x d e f

modifyLocalsMap :: (LocalsMap -> LocalsMap) -> ExprGen ()
modifyLocalsMap f = do
    x <- getLocalsMap
    putLocalsMap $ f x

getType :: ExprGen L.Type
getType = do
    (ExprGenData _ _ _ _ _ _ x) <- get
    return x

putType :: L.Type -> ExprGen ()
putType x = do
    (ExprGenData a b c d e f _) <- get
    put $ ExprGenData a b c d e f x

modifyType :: (L.Type -> L.Type) -> ExprGen ()
modifyType f = do
    x <- getType
    putType $ f x

addLocal :: L.Type -> ExprGen Int
addLocal t = do
    len <- getLocalsLen
    modifyLocalsLen (+ 1)
    modifyLocals $ (flip D.snoc . typeToValueType) t
    return len

addNamedLocal :: L.Type -> L.Ident -> ExprGen Int
addNamedLocal t name = do
    id <- addLocal t
    modifyLocalsMap (M.insert name (t, id))
    return id

getNamedLocal :: L.Ident -> ExprGen LocalData
getNamedLocal name = do
    x <- getLocalsMap
    return $ x M.! name

addOpCode :: W.OperatorCode -> ExprGen ()
addOpCode x = modifyOpCodes $ (flip D.snoc) x

callGen :: FunctionMap -> String -> [ExprGen ()] -> ExprGen ()
callGen m f args = do
    sequence_ args
    addOpCode $ W.OpCall $ m M.! f

blockGen :: W.BlockType -> ExprGen () -> ExprGen ()
blockGen t x = do
    addOpCode $ W.OpBlock t
    x
    addOpCode $ W.OpEnd

exprGen :: E.Expr -> ExprGen ()
exprGen expr = case expr of
    E.EStructL name exprs -> blockGen (W.BlockType (Just W.ValI32)) $ do
        sDef <- (M.! name) <$> getStructs
        let exprMap = M.fromList exprs
        callGen fMap
                (name ++ ":new")
                (map (exprGen mMap lMap . (exprMap M.!)) sDef)
    E.EI32L x -> addOpCode $ W.OpI32Const x
    E.EI64L x -> addOpCode $ W.OpI64Const x
    E.EF32L x -> addOpCode $ W.OpF32Const x
    E.EF64L x -> addOpCode $ W.OpF64Const x

typeToValueType :: L.Type -> W.ValueType
typeToValueType L.TI32 = W.ValI32
typeToValueType L.TI64 = W.ValI64
typeToValueType L.TF32 = W.ValF32
typeToValueType L.TF64 = W.ValF64
typeToValueType _      = W.ValI32
