module Argon.Visitor (funcsCC)
    where

import Data.Maybe (fromJust)
import Data.Generics (Data, Typeable, everything, mkQ, extQ)

import qualified GHC
import qualified RdrName as GHC
import qualified OccName as GHC

import Argon.Span
import Argon.Types (ComplexityBlock(..))

type Exp = GHC.HsExpr GHC.RdrName
type Function = GHC.HsBindLR GHC.RdrName GHC.RdrName
type MatchBody = GHC.LHsExpr GHC.RdrName


-- | Compute cyclomatic complexity of every function binding in the given AST.
funcsCC :: (Data from, Typeable from) => from -> [ComplexityBlock]
funcsCC = map funCC . getBinds

funCC :: Function -> ComplexityBlock
funCC f = CC (getLocation $ GHC.fun_id f, getFuncName f, complexity f)

getBinds :: (Data from, Typeable from) => from -> [Function]
getBinds = everything (++) $ mkQ [] visit
    where visit fun@(GHC.FunBind {}) = [fun]
          visit _ = []

getLocation :: GHC.Located a -> Span
getLocation = srcSpanToSpan . GHC.getLoc

-- TODO: make it safe
getFuncName :: Function -> String
getFuncName = GHC.occNameString . GHC.rdrNameOcc . GHC.unLoc . fst . fromJust
            . GHC.m_fun_id_infix . GHC.unLoc . head . getMatches

-- XXX: does not work
complexity :: Function -> Int
complexity f = let matches = getMatches f
                   query = everything (+) $ 0 `mkQ` visitExp `extQ` visitOp
                in length matches + sumWith query matches

getMatches :: Function -> [GHC.LMatch GHC.RdrName MatchBody]
getMatches = GHC.mg_alts . GHC.fun_matches

sumWith :: (a -> Int) -> [a] -> Int
sumWith f = sum . map f

visitExp :: Exp -> Int
visitExp (GHC.HsIf {}) = 1
visitExp (GHC.HsMultiIf _ alts) = length alts - 1
visitExp (GHC.HsLamCase _ alts) = length (GHC.mg_alts alts) - 1
visitExp (GHC.HsCase _ alts)    = length (GHC.mg_alts alts) - 1
visitExp _ = 0

-- TODO: add implementation
visitOp :: Exp -> Int
visitOp _ = 0

-- XXX: Is it really worth it?
isFunction :: Function -> Bool
isFunction f = True  -- TODO: find a way to recognize if a bind is a function
