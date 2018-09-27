import           Wasm.AST
import           Wasm.Binary
import           Data.Serialize
import qualified Data.ByteString               as BS

ast :: WasmASTRoot
ast = WasmASTRoot
    (Just (TypeSection [FuncType [ValI32] (Just ValI32)]))
    Nothing
    (Just (FunctionSection [0]))
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    (Just (CodeSection [FunctionBody [] [OpGetLocal 0, OpI32Const 1, OpI32Add]])
    )
    Nothing

main = do
    let bin = runPut $ putWasmAST ast
    BS.writeFile "test.wasm" bin
    return ()
