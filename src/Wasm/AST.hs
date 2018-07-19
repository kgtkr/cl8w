module Wasm.AST where
import qualified Data.Binary                   as B
import qualified Data.ByteString               as BS
import           Data.Int
import           Data.Binary.Get

data ValueType=I32|I64|F32|F64

data Type=ValueType ValueType|AnyFunc|Func|BlockType (Maybe ValueType)

