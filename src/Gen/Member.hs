module Gen.Member where

import           Control.Lens
import qualified Data.Map                      as M
import qualified Parsers.Member                as PM

import           Gen.Lang                      as GL
import           Gen.Expr                      as GE
import           Gen.Stat                      as GS

import           Data.List

import qualified Wasm.AST                      as WA

import qualified Data.DList                    as D
import           Data.Maybe
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Gen.OpCodeGen                 as GO

fromASTStruct :: PM.StructMembers -> Struct
fromASTStruct ms = M.fromList (f 0 (sortOn fst ms))
  where
    f pos ((ident, t) : xs) =
        (ident, StructProps pos t ident) : (f (pos + GL.sizeOf t) xs)
    f _ [] = []

toMemberData :: [PM.Member] -> MemberData
toMemberData ms = MemberData
    { _memberDataStructs   = undefined
    , _memberDataFunctions = ( M.fromList
                             . map
                                   (\(i, d@(PM.FuncDef name _ _)) ->
                                       (name, (i, d))
                                   )
                             . zip [0 ..]
                             . mapMaybe
                                   (\case
                                       PM.MFun       d _ -> Just d
                                       PM.MExternFun d _ -> Just d
                                   )
                             )
        ms
    }

data MemberGenData=MemberGenData{
    _memberGenDataDefineFunctionsLen::Int,
    _memberGenDataExternFunctionsLen::Int,
    _memberGenDataTypeSections::D.DList WA.FuncType,
    _memberGenDataImportSections::D.DList WA.ImportEntry,
    _memberGenDataFunctionSections::D.DList Int,
    _memberGenDataExportSections::D.DList WA.ExportEntry,
    _memberGenDataCodeSections::D.DList WA.FunctionBody
}
makeFields ''MemberGenData

memberGenData = MemberGenData
    { _memberGenDataDefineFunctionsLen = 0
    , _memberGenDataExternFunctionsLen = 0
    , _memberGenDataTypeSections       = D.empty
    , _memberGenDataImportSections     = D.empty
    , _memberGenDataFunctionSections   = D.empty
    , _memberGenDataExportSections     = D.empty
    , _memberGenDataCodeSections       = D.empty
    }

type MemberGen=State MemberGenData

fDefToType :: PM.FuncDef -> WA.FuncType
fDefToType (PM.FuncDef _ params ret) = WA.FuncType
    (fmap (GL.typeToValueType . snd) params)
    (fmap GL.typeToValueType ret)

compile :: [PM.Member] -> WA.WasmASTRoot
compile x = WA.wasmASTRootDefault
    { WA._wasmASTRootTypeSection     =
        (Just . WA.TypeSection . D.toList . (^. typeSections)) res
    , WA._wasmASTRootImportSection   =
        (Just . WA.ImportSection . D.toList . (^. importSections)) res
    , WA._wasmASTRootFunctionSection =
        (Just . WA.FunctionSection . D.toList . (^. functionSections)) res
    , WA._wasmASTRootExportSection   =
        (Just . WA.ExportSection . D.toList . (^. exportSections)) res
    , WA._wasmASTRootCodeSection     =
        (Just . WA.CodeSection . D.toList . (^. codeSections)) res
    }
  where
    md  = toMemberData x
    res = execState (membersGen md x) memberGenData

membersGen :: MemberData -> [PM.Member] -> MemberGen ()
membersGen md = mapM_ (memberGen md)

memberGen :: MemberData -> PM.Member -> MemberGen ()
memberGen md (PM.MFun d@(PM.FuncDef name params ret) stat) = do
    functionIndex <- (+) <$> use defineFunctionsLen <*> use externFunctionsLen
    defineFunctionsLen += 1
    typeSections %= (`D.snoc` fDefToType d)
    functionSections %= (`D.snoc` functionIndex)
    exportSections %= (`D.snoc` WA.ExportEntry name WA.ExFunction functionIndex)
    let x = GO.emptyOpCodeGenData params
    let s = execState (runReaderT (GS.statGen stat) md) x
    codeSections
        %= (`D.snoc` WA.FunctionBody
               ((map (WA.LocalEntry 1) . D.toList . (^. GO.locals)) s)
               ((D.toList . (^. GO.opCodes)) s)
           )
    return ()
