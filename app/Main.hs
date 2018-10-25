import           Wasm.AST
import           Wasm.Binary
import           Data.Serialize
import qualified Data.ByteString               as BS
import           Wasm.Parser

main = do
    input <- readFile "test.cl8w"
    let bin = runPut $ putWasmAST ast
    BS.writeFile "test.wasm" bin
    return ()
