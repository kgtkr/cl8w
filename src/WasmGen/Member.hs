{-# LANGUAGE TemplateHaskell #-}

module WasmGen.Member where

import qualified Parsers.Lang                  as L
import           Control.Lens
import qualified Data.Map                      as M
import qualified Parsers.Member                as Me

import           WasmGen.Lang                  as WL
import           WasmGen.Expr                  as WE
import           WasmGen.Stat                  as WS

import           Data.List

import qualified Wasm.AST                      as WA

import qualified Data.DList                    as D
import           Data.Maybe
import           Control.Monad.State
import           Control.Monad.Reader

fromASTStruct :: Me.StructMembers -> Struct
fromASTStruct ms = M.fromList (f 0 (sortOn fst ms))
  where
    f pos ((ident, t) : xs) =
        (ident, StructProps pos t ident) : (f (pos + WL.sizeOf t) xs)
    f _ [] = []

toMemberData :: [Me.Member] -> MemberData
toMemberData ms = MemberData
    { _structs   = undefined
    , _functions = ( M.fromList
                   . map (\(i, d@(Me.FuncDef name _ _)) -> (name, (i, d)))
                   . zip [0 ..]
                   . mapMaybe
                         (\m -> case m of
                             Me.MFun       d _ -> Just d
                             Me.MExternFun d _ -> Just d
                         )
                   )
        ms
    }

data MemberGenData=MemberGenData{
    _defineFunctionsLen::Int,
    _externFunctionsLen::Int,
    _typeSections::D.DList WA.FuncType,
    _importSections::D.DList WA.ImportEntry,
    _functionSections::D.DList Int,
    _exportSections::D.DList WA.ExportEntry,
    _codeSections::D.DList WA.FunctionBody
}
makeLenses ''MemberGenData

memberGenData = MemberGenData
    { _defineFunctionsLen = 0
    , _externFunctionsLen = 0
    , _typeSections       = D.empty
    , _importSections     = D.empty
    , _functionSections   = D.empty
    , _exportSections     = D.empty
    , _codeSections       = D.empty
    }

type MemberGen=State MemberGenData

fDefToType :: Me.FuncDef -> WA.FuncType
fDefToType (Me.FuncDef _ params ret) = WA.FuncType
    (fmap (WL.typeToValueType . snd) params)
    (fmap WL.typeToValueType ret)

compile :: [Me.Member] -> WA.WasmASTRoot
compile x = WA.WasmASTRoot
    ((Just . WA.TypeSection . D.toList . _typeSections) res)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
  where
    md       = toMemberData x
    (_, res) = runState (membersGen md x) memberGenData

membersGen :: MemberData -> [Me.Member] -> MemberGen ()
membersGen md = mapM_ (memberGen md)

memberGen :: MemberData -> Me.Member -> MemberGen ()
memberGen md (Me.MFun d@(Me.FuncDef name params ret) stat) = do
    functionIndex <- (+) <$> use defineFunctionsLen <*> use externFunctionsLen
    defineFunctionsLen += 1
    typeSections %= (flip D.snoc) (fDefToType d)
    functionSections %= (flip D.snoc) functionIndex
    exportSections
        %= (flip D.snoc) (WA.ExportEntry name WA.ExFunction functionIndex)
    let x      = WE.emptyExprGenData params
    let (_, s) = runState ((runReaderT (WS.statGen stat)) md) x
    codeSections %= (flip D.snoc)
        (WA.FunctionBody
            ((map (\x -> WA.LocalEntry 1 x) . D.toList . WE._locals) s)
            ((D.toList . WE._opCodes) s)
        )
    return ()
