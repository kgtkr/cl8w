{-# LANGUAGE TemplateHaskell #-}


module WasmGen.Stat where

import qualified Data.DList                    as D
import qualified WasmGen.Expr                  as GE
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Parsers.Expr                  as PE
import qualified Parsers.Stat                  as PS
import qualified Data.Map                      as M
import qualified Wasm.AST                      as WA

statGen :: PS.Stat -> GE.ExprGen ()
statGen (PS.SBlock      s) = mapM_ statGen s
statGen (PS.SExprToStat e) = do
    t <- GE.exprType e
    case t of
        Just _ -> do
            GE.exprGen e
            GE.addOpCode $ WA.OpDrop
        Nothing -> GE.exprGen e
statGen (PS.SLet name t e) = do
    x <- GE.addNamedLocalData t name
    GE.exprGen e
    GE.addOpCode $ WA.OpSetLocal x
statGen (PS.SIf (e, s1) [] (Just s2)) = do
    GE.exprGen e
    GE.addOpCode $ WA.OpIf $ WA.BlockType Nothing
    statGen s1
    GE.addOpCode $ WA.OpElse
    statGen s2
    GE.addOpCode $ WA.OpEnd
statGen (PS.SReturn e) = do
    case e of
        Just e  -> GE.exprGen e
        Nothing -> return ()
    GE.addOpCode $ WA.OpReturn
