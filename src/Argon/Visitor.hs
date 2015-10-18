module Argon.Visitor
    where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (childrenBi, universeBi)
import Language.Haskell.Exts.Syntax

type ComplexityResult = (String, SrcLoc, Int)


funCC :: Decl -> ComplexityResult
funCC f@(FunBind (Match loc n _ _ _ _:_)) = (name n, loc, complexity f)
    where name (Ident s)   = s
          name (Symbol s) = s
funCC _ = ("<unknown>", SrcLoc "<unknown>.hs" 0 0, 0)

sumWith :: (a -> Int) -> [a] -> Int
sumWith f = sum . map f

complexity :: Data from => from -> Int
complexity node = 1 + visitMatches node + visitExps node

visitMatches :: Data from => from -> Int
visitMatches = sumWith descend . childrenBi
    where descend :: [Match] -> Int
          descend x = length x - 1 + sumWith visitMatches x

visitExps :: Data from => from -> Int
visitExps = sumWith pathCount . universeBi
    where pathCount :: Exp -> Int
          pathCount (If {})        = 1
          pathCount (MultiIf alts) = length alts - 1
          pathCount (Case _ alts)  = length alts - 1
          pathCount (LCase alts)   = length alts - 1
          pathCount (InfixApp _ (QVarOp (UnQual (Symbol op))) _) =
              case op of
                "||" -> 1
                "&&" -> 1
                _    -> 0
          pathCount _ = 0
