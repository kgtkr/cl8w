module WasmGen.Expr where

import qualified Data.DList                    as D
import qualified Wasm.AST                      as W
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Parsers.Member                as Me
import qualified Parsers.Lang                  as L

data ExprGenData=ExprGenData OpCodes Locals LocalsLen LocalsMap FunctionMap StructMap
type FunctionMap=M.Map String Int
type StructMap=M.Map String Me.StructMembers
type OpCodes=D.DList W.OperatorCode
type Locals=D.DList W.ValueType
type LocalsLen=Int
type LocalsMap=M.Map String LocalData
type LocalData=(W.ValueType,Int)
type ExprGen = State ExprGenData

getFunctions :: ExprGen FunctionMap
getFunctions = do
    (ExprGenData _ _ _ _ x _) <- get
    return x

getStructs :: ExprGen StructMap
getStructs = do
    (ExprGenData _ _ _ _ _ x) <- get
    return x

getOpCodes :: ExprGen OpCodes
getOpCodes = do
    (ExprGenData x _ _ _ _ _) <- get
    return x

putOpCpdes :: OpCodes -> ExprGen ()
putOpCpdes x = do
    (ExprGenData _ a b c d e) <- get
    put $ ExprGenData x a b c d e

modifyOpCodes :: (OpCodes -> OpCodes) -> ExprGen ()
modifyOpCodes f = do
    x <- getOpCodes
    putOpCpdes $ f x

getLocals :: ExprGen Locals
getLocals = do
    (ExprGenData _ x _ _ _ _) <- get
    return x

putLocals :: Locals -> ExprGen ()
putLocals x = do
    (ExprGenData a _ b c d e) <- get
    put $ ExprGenData a x b c d e

modifyLocals :: (Locals -> Locals) -> ExprGen ()
modifyLocals f = do
    x <- getLocals
    putLocals $ f x

getLocalsLen :: ExprGen LocalsLen
getLocalsLen = do
    (ExprGenData _ _ x _ _ _) <- get
    return x

putLocalsLen :: LocalsLen -> ExprGen ()
putLocalsLen x = do
    (ExprGenData a b _ c d e) <- get
    put $ ExprGenData a b x c d e

modifyLocalsLen :: (LocalsLen -> LocalsLen) -> ExprGen ()
modifyLocalsLen f = do
    x <- getLocalsLen
    putLocalsLen $ f x

getLocalsMap :: ExprGen LocalsMap
getLocalsMap = do
    (ExprGenData _ _ _ x _ _) <- get
    return x

putLocalsMap :: LocalsMap -> ExprGen ()
putLocalsMap x = do
    (ExprGenData a b c _ d e) <- get
    put $ ExprGenData a b c x d e

modifyLocalsMap :: (LocalsMap -> LocalsMap) -> ExprGen ()
modifyLocalsMap f = do
    x <- getLocalsMap
    putLocalsMap $ f x

addLocal :: W.ValueType -> ExprGen Int
addLocal t = do
    len <- getLocalsLen
    modifyLocalsLen (+ 1)
    modifyLocals $ (flip D.snoc) t
    return len

addNamedLocal :: W.ValueType -> L.Ident -> ExprGen Int
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
