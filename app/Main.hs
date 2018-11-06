import           Wasm.AST
import           Wasm.Binary
import           Data.Serialize
import qualified Data.ByteString               as BS
import           Parsers.Member
import           Text.ParserCombinators.Parsec
import           Gen.Member
import           System.Environment             ( getArgs )

main = do
    [fileName] <- getArgs
    input      <- readFile fileName
    let ast = (parse moduleP fileName input)
    case ast of
        Right ast -> do
            -- print ast
            let wAST = compile ast
            let bin  = runPut $ putWasmAST wAST
            BS.writeFile "main.wasm" bin
        Left e -> print e
    return ()
