module Gen.Gen where

import qualified Wasm.AST                      as W
import qualified Parsers.Expr                  as E
import qualified Data.Map                      as M
import qualified Parsers.Lang                  as L

import qualified Parsers.Member                as Me
import           Data.Tuple                     ( swap )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.DList                    as D
import           Control.Monad.Writer

type MemberMap=(FunctionMap,StructMap)

type StructMap=M.Map String Me.StructMembers

memberMap :: [Me.Member] -> MemberMap
memberMap m =
  ( (M.fromList . map swap . zip [0 ..] . mapMaybe fnMap) m
  , (M.fromList . mapMaybe stMap) m
  )
 where
  fnMap (Me.MFun (Me.FuncDef name _ _) _) = Just name
  fnMap (Me.MExternFun (Me.FuncDef name _ _) _) = Just name
  fnMap _ = Nothing
  stMap (Me.MStruct a b) = Just (a, b)
  stMap _                = Nothing

type FunctionMap=M.Map String Int
