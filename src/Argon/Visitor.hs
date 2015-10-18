module Argon.Visitor
    where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (childrenBi, universeBi)
import Language.Haskell.Exts.Syntax

type ComplexityResult = (String, Int, Int, Int)


funcsCC :: Data from => from -> [ComplexityResult]
funcsCC ast = map funCC [matches | FunBind matches <- universeBi ast]

funCC :: [Match] -> ComplexityResult
funCC [] = ("<unknown>", 0, 0, 0)
funCC ms@(Match (SrcLoc _ l c) n _ _ _ _:_) = (name n, l, c, complexity ms)
    where name (Ident s)   = s
          name (Symbol s) = s

sumWith :: (a -> Int) -> [a] -> Int
sumWith f = sum . map f

complexity :: Data from => from -> Int
complexity node = 1 + visitMatches node + visitExps node

visitMatches :: Data from => from -> Int
visitMatches = sumWith descend . childrenBi
    where descend :: [Match] -> Int
          descend x = length x - 1 + sumWith visitMatches x

visitExps :: Data from => from -> Int
visitExps = sumWith visitExp . universeBi

visitExp :: Exp -> Int
visitExp (If {})        = 1
visitExp (MultiIf alts) = length alts - 1
visitExp (Case _ alts)  = length alts - 1
visitExp (LCase alts)   = length alts - 1
visitExp (InfixApp _ (QVarOp (UnQual (Symbol op))) _) =
  case op of
    "||" -> 1
    "&&" -> 1
    _    -> 0
visitExp _ = 0
