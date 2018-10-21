module WasmGen.Gen where

import qualified Wasm.AST                      as W
import qualified Parsers.Expr                  as E
import qualified Data.Map                      as M
import qualified Parsers.Lang                  as L

import qualified Parsers.Member                as Me
import           Data.Tuple                     ( swap )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.DList                    as D
import           Control.Monad.Writer

type MemberMap=(FunctionMap,StructMap)

type StructMap=M.Map String Me.StructMembers

memberMap :: [Me.Member] -> MemberMap
memberMap m =
    ( (M.fromList . map swap . zip [0 ..] . mapMaybe fnMap) m
    , (M.fromList . mapMaybe stMap) m
    )
  where
    fnMap (Me.MFun (Me.FuncDef name _ _) _) = Just name
    fnMap (Me.MExternFun (Me.FuncDef name _ _) _) = Just name
    fnMap _ = Nothing
    stMap (Me.MStruct a b) = Just (a, b)
    stMap _                = Nothing

type FunctionMap=M.Map String Int

type LocalVarMap=M.Map String (Int,L.Type)

type OpCodeWriter = Writer (D.DList W.OperatorCode) ()

tellOp :: W.OperatorCode -> OpCodeWriter
tellOp = tell . pure

callGen :: FunctionMap -> String -> [OpCodeWriter] -> OpCodeWriter
callGen map f args = do
    sequence_ args
    tellOp $ W.OpCall $ map M.! f
    return ()

blockGen :: W.BlockType -> OpCodeWriter -> OpCodeWriter
blockGen t x = do
    tell $ pure $ W.OpBlock t
    x
    tell $ pure $ W.OpEnd

exprGen :: MemberMap -> LocalVarMap -> E.Expr -> OpCodeWriter
exprGen mMap@(fMap, sMap) lMap expr = case expr of
    E.EStructL name exprs -> blockGen (W.BlockType (Just W.ValI32)) $ do
        let sDef    = map fst $ sMap M.! name
        let exprMap = M.fromList exprs
        callGen fMap
                (name ++ ":new")
                (map (exprGen mMap lMap . (exprMap M.!)) sDef)
    E.EI32L x -> tellOp $ W.OpI32Const x
    E.EI64L x -> tellOp $ W.OpI64Const x
    E.EF32L x -> tellOp $ W.OpF32Const x
    E.EF64L x -> tellOp $ W.OpF64Const x
