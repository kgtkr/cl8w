import           Wasm.AST
import           Wasm.Binary
import           Data.Serialize
import qualified Data.ByteString               as BS
import           Parsers.Member
import           Text.ParserCombinators.Parsec
import           Gen.Member

main = do
    input <- readFile "test.cl8w"
    let ast = (parse moduleP "test" input)
    case ast of
        Right ast -> do
            -- print ast
            let wAST = compile ast
            let bin  = runPut $ putWasmAST wAST
            BS.writeFile "test.wasm" bin
        Left e -> print e
    return ()
