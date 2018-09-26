module Wasm.Binary where

import           Wasm.AST

opCode :: Code -> Int
opCode Unreachable = 0x00
opCode Nop         = 0x01
