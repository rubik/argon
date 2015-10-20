module Argon.Visitor (funcsCC)
    where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (childrenBi, universeBi)
import Language.Haskell.Exts.Syntax
import Argon.Types (ComplexityBlock)


-- | Compute cyclomatic complexity of every function binding in the given AST.
funcsCC :: Data from => from -> [ComplexityBlock]
funcsCC ast = map funCC [matches | FunBind matches <- universeBi ast]

funCC :: [Match] -> ComplexityBlock
funCC [] = (0, 0, "<unknown>", 0)
funCC ms@(Match (SrcLoc _ l c) n _ _ _ _:_) = (l, c, name n, complexity ms)
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
visitExps = sumWith inspect . universeBi
    where inspect e = visitExp e + visitOp e

visitExp :: Exp -> Int
visitExp (If {})        = 1
visitExp (MultiIf alts) = length alts - 1
visitExp (Case _ alts)  = length alts - 1
visitExp (LCase alts)   = length alts - 1
visitExp _ = 0

visitOp :: Exp -> Int
visitOp (InfixApp _ (QVarOp (UnQual (Symbol op))) _) =
  case op of
    "||" -> 1
    "&&" -> 1
    _    -> 0
visitOp _ = 0
