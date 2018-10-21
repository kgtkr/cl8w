module WasmGen.Expr where

import           Data.Monoid
import           Data.Semigroup
import qualified Data.DList                    as D
import qualified Wasm.AST                      as W
import qualified Data.Map                      as M
import           Control.Monad.State
data ExprGenData=ExprGenData OpCodes Locals LocalsLen LocalsMap

type OpCodes=D.DList W.OperatorCode
type Locals=D.DList W.ValueType
type LocalsLen=Int
type LocalsMap=M.Map String Int

instance Monoid ExprGenData where
    mempty = ExprGenData mempty mempty 0 mempty
    ExprGenData a1 a2 a3 a4 `mappend` ExprGenData b1 b2 b3 b4=ExprGenData (mappend a1 b1) (mappend a2 b2) (a3 + b3) (mempty a4 b4)

type ExprGen = State ExprGenData

getOpCodes :: ExprGen OpCodes
getOpCodes = do
    (ExprGenData x _ _ _) <- get
    return x

putOpCpdes :: OpCodes -> ExprGen ()
putOpCpdes x = do
    (ExprGenData _ a b c) <- get
    put $ ExprGenData x a b c

modifyOpCodes :: (OpCodes -> OpCodes) -> ExprGen ()
modifyOpCodes f = do
    x <- getOpCodes
    putOpCpdes $ f x

getLocals :: ExprGen Locals
getLocals = do
    (ExprGenData _ x _ _) <- get
    return x

putLocals :: Locals -> ExprGen ()
putLocals x = do
    (ExprGenData a _ b c) <- get
    put $ ExprGenData a x b c

modifyLocals :: (Locals -> Locals) -> ExprGen ()
modifyLocals f = do
    x <- getLocals
    putLocals $ f x

getLocalsLen :: ExprGen LocalsLen
getLocalsLen = do
    (ExprGenData _ _ x _) <- get
    return x

putLocalsLen :: LocalsLen -> ExprGen ()
putLocalsLen x = do
    (ExprGenData a b _ c) <- get
    put $ ExprGenData a b x c

modifyLocalsLen :: (LocalsLen -> LocalsLen) -> ExprGen ()
modifyLocalsLen f = do
    x <- getLocalsLen
    putLocalsLen $ f x

getLocalsMap :: ExprGen LocalsMap
getLocalsMap = do
    (ExprGenData _ _ _ x) <- get
    return x

putLocalsMap :: LocalsMap -> ExprGen ()
putLocalsMap x = do
    (ExprGenData a b c _) <- get
    put $ ExprGenData a b c x

modifyLocalsMap :: (LocalsMap -> LocalsMap) -> ExprGen ()
modifyLocalsMap f = do
    x <- getLocalsMap
    putLocalsMap $ f x

addLocal :: W.ValueType -> ExprGen LocalsLen
addLocal t = do
    len <- getLocalsLen
    modifyLocalsLen (+ 1)
    modifyLocals $ (flip D.snoc) t
    return len
