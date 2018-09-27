import           Wasm.AST
import           Wasm.Binary
import           Data.Serialize
import qualified Data.ByteString               as BS

ast :: WasmASTRoot
ast = WasmASTRoot (Just (TypeSection [FuncType [ValI32] (Just ValI32)]))
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

main = do
    let bin = runPut $ putWasmAST ast
    BS.writeFile "test.wasm" bin
    return ()
