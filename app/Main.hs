import           Parsers
import           Text.ParserCombinators.Parsec

main = print $ parse exprP "test" "Struct {x:Foo{},y:f(),}"
