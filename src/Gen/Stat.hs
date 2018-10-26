module Gen.Stat where

import qualified Data.DList                    as D
import qualified Gen.Expr                      as GE
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Parsers.Expr                  as PE
import qualified Parsers.Stat                  as PS
import qualified Data.Map                      as M
import qualified Wasm.AST                      as WA
import qualified Gen.OpCodeGen                 as GO

statGen :: PS.Stat -> GO.OpCodeGen ()
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
statGen (PS.SIf (e, s1) [] s2) = do
    GE.exprGen e
    GE.addOpCode $ WA.OpIf $ WA.BlockType Nothing
    statGen s1
    case s2 of
        Just s2 -> do
            GE.addOpCode WA.OpElse
            statGen s2
        Nothing -> return ()
    GE.addOpCode $ WA.OpEnd
statGen (PS.SReturn e) = do
    forM_ e GE.exprGen
    GE.addOpCode $ WA.OpReturn
