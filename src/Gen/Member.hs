module Gen.Member where

import           Control.Lens
import qualified Data.Map                      as M
import qualified Parsers.Member                as PM

import           Gen.Lang                      as GL
import           Gen.Expr                      as GE
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
        (ident, StructProp pos t ident) : (f (pos + GL.sizeOf t) xs)
    f _ [] = []

toMemberData :: [PM.Member] -> MemberData
toMemberData ms = MemberData
    { _memberDataStructs   = (M.fromList . map (over _2 fromASTStruct)) structs
    , _memberDataFunctions = M.fromList
        (fdToMap 0 externFunDefs ++ fdToMap (length externFunDefs) funDefs)
    }
  where
    fdToMap initID =
        map (\(i, fd) -> (fd ^. PM.name, (i, fd))) . zip [initID ..]
    funDefs = mapMaybe
        (\case
            PM.MFun fd _ -> Just fd
            _            -> Nothing
        )
        ms
    externFunDefs = mapMaybe
        (\case
            PM.MExternFun fd _ _ -> Just fd
            _                    -> Nothing
        )
        ms
    structs = mapMaybe
        (\case
            PM.MStruct name member -> Just (name, member)
            _                      -> Nothing
        )
        ms

data MemberGenData=MemberGenData{
    _memberGenDataDefineFunctionsLen::Int,
    _memberGenDataExternFunctionsLen::Int,
    _memberGenDataTypeSection::D.DList WA.FuncType,
    _memberGenDataImportSection::D.DList WA.ImportEntry,
    _memberGenDataFunctionSection::D.DList Int,
    _memberGenDataExportSection::D.DList WA.ExportEntry,
    _memberGenDataCodeSection::D.DList WA.FunctionBody
}
makeFields ''MemberGenData

memberGenData = MemberGenData
    { _memberGenDataDefineFunctionsLen = 0
    , _memberGenDataExternFunctionsLen = 0
    , _memberGenDataTypeSection        = D.empty
    , _memberGenDataImportSection      = D.empty
    , _memberGenDataFunctionSection    = D.empty
    , _memberGenDataExportSection      = D.empty
    , _memberGenDataCodeSection        = D.empty
    }

type MemberGen=State MemberGenData

fDefToType :: PM.FuncDef -> WA.FuncType
fDefToType fd = WA.FuncType
    (fmap (GL.typeToValueType . snd) (fd ^. PM.params))
    (fmap GL.typeToValueType (fd ^. PM.result))

compile :: PM.Module -> WA.WasmASTRoot
compile x = WA.wasmASTRootDefault
    { WA._wasmASTRootTypeSection     =
        (Just . WA.TypeSection . D.toList . (^. typeSection)) res
    , WA._wasmASTRootImportSection   =
        ( Just
            . WA.ImportSection
            . ((WA.ImportEntry
                   "resource"
                   "memory"
                   (WA.ExImExMemory
                       (WA.MemoryType (WA.ResizableLimits 10 Nothing))
                   )
               ) :
              )
            . D.toList
            . (^. importSection)
            )
            res
    , WA._wasmASTRootFunctionSection =
        (Just . WA.FunctionSection . D.toList . (^. functionSection)) res
    , WA._wasmASTRootExportSection   =
        (Just . WA.ExportSection . D.toList . (^. exportSection)) res
    , WA._wasmASTRootCodeSection     =
        (Just . WA.CodeSection . D.toList . (^. codeSection)) res
    , WA._wasmASTRootTableSection    =
        Just $ WA.TableSection
            [WA.TableType WA.ElAnyFunc (WA.ResizableLimits (length x) Nothing)]
    , WA._wasmASTRootElementSection  =
        Just $ WA.ElementSection
            [WA.ElemSegment (WA.InitI32 0) [0 .. length x - 1]]
    }
  where
    md  = toMemberData x
    res = execState (membersGen md x) memberGenData

membersGen :: MemberData -> [PM.Member] -> MemberGen ()
membersGen md = mapM_ (memberGen md)

memberGen :: MemberData -> PM.Member -> MemberGen ()
memberGen md (PM.MFun fd stat) = do
    functionIndex <- (+) <$> use defineFunctionsLen <*> use externFunctionsLen
    defineFunctionsLen += 1
    typeSection %= (`D.snoc` fDefToType fd)
    functionSection %= (`D.snoc` functionIndex)
    exportSection
        %= (`D.snoc` WA.ExportEntry (fd ^. PM.name) WA.ExFunction functionIndex)
    let x  = GO.emptyOpCodeGenData (fd ^. PM.params)
    let et = evalState (runReaderT (GE.exprType stat) md) x
    let s  = execState (runReaderT (GE.exprGen stat) md) x
    let opCodeF = case (fd ^. PM.result, et) of
            (Just _, Nothing) -> (`D.snoc` WA.OpUnreachable)
            _                 -> id

    codeSection
        %= (`D.snoc` WA.FunctionBody
               ((map (WA.LocalEntry 1) . D.toList . (^. GO.locals)) s)
               ((D.toList . opCodeF . (^. GO.opCodes)) s)
           )
    return ()
memberGen _  (PM.MStruct _ _              ) = return ()
memberGen md (PM.MExternFun fd name1 name2) = do
    functionIndex <- (+) <$> use defineFunctionsLen <*> use externFunctionsLen
    externFunctionsLen += 1
    typeSection %= (`D.snoc` fDefToType fd)
    importSection
        %= (`D.snoc` (WA.ImportEntry name1 name2 (WA.ExImFunction functionIndex)
                     )
           )

    return ()
