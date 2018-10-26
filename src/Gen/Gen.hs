module Gen.Gen where

import qualified Data.Map                      as M

import qualified Parsers.Member                as PM
import           Data.Tuple                     ( swap )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.DList                    as D
import           Control.Monad.Writer

type MemberMap=(FunctionMap,StructMap)

type StructMap=M.Map String PM.StructMembers

memberMap :: [PM.Member] -> MemberMap
memberMap m =
  ( (M.fromList . map swap . zip [0 ..] . mapMaybe fnMap) m
  , (M.fromList . mapMaybe stMap) m
  )
 where
  fnMap (PM.MFun (PM.FuncDef name _ _) _) = Just name
  fnMap (PM.MExternFun (PM.FuncDef name _ _) _) = Just name
  fnMap _ = Nothing
  stMap (PM.MStruct a b) = Just (a, b)
  stMap _                = Nothing

type FunctionMap=M.Map String Int