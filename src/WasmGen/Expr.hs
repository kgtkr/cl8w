module WasmGen.Expr where

import qualified Data.DList                    as D
import qualified Wasm.AST                      as W
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Parsers.Member                as Me
import qualified Parsers.Lang                  as L
import qualified Parsers.Expr                  as E

data ExprGenData=ExprGenData OpCodes Locals LocalsLen LocalsMap FunctionMap StructMap ExprType
type FunctionMap=M.Map String (Int,Me.FuncDef)
type StructMap=M.Map String Me.StructMembers
type OpCodes=D.DList W.OperatorCode
type Locals=D.DList W.ValueType
type LocalsLen=Int
type LocalsMap=M.Map String LocalData
type LocalData=(L.Type,Int)
type ExprGen = State ExprGenData

type ExprType=Maybe L.Type

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

getType :: ExprGen ExprType
getType = do
    (ExprGenData _ _ _ _ _ _ x) <- get
    return x

putType :: ExprType -> ExprGen ()
putType x = do
    (ExprGenData a b c d e f _) <- get
    put $ ExprGenData a b c d e f x

modifyType :: (ExprType -> ExprType) -> ExprGen ()
modifyType f = do
    x <- getType
    putType $ f x

addLocal :: L.Type -> ExprGen Int
addLocal t = do
    len <- getLocalsLen
    modifyLocalsLen (+ 1)
    modifyLocals $ (flip D.snoc . typeToValueType) t
    return len

addNamedLocalData :: L.Type -> L.Ident -> ExprGen Int
addNamedLocalData t name = do
    id <- addLocal t
    modifyLocalsMap (M.insert name (t, id))
    return id

getNamedLocalData :: L.Ident -> ExprGen LocalData
getNamedLocalData name = do
    x <- getLocalsMap
    return $ x M.! name

addOpCode :: W.OperatorCode -> ExprGen ()
addOpCode x = modifyOpCodes $ (flip D.snoc) x

callGen :: FunctionMap -> String -> [ExprGen ()] -> ExprGen ()
callGen m f args = do
    sequence_ args
    let (id, Me.FuncDef _ _ re) = m M.! f
    addOpCode $ W.OpCall id
    putType $ re

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
        sDef <- (M.! name) <$> getStructs
        putType $ (Just . L.RefType . L.TStruct) name
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
