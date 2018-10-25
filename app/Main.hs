import           Wasm.AST
import           Wasm.Binary
import           Data.Serialize
import qualified Data.ByteString               as BS
import           Parsers.Member
import           Text.ParserCombinators.Parsec
import           WasmGen.Member

main = do
    input <- readFile "test.cl8w"
    let Right ast = (parse membersP "test" input)
    let wAST      = compile ast
    let bin       = runPut $ putWasmAST wAST
    BS.writeFile "test.wasm" bin
    return ()
