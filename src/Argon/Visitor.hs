module Argon.Visitor (funcsCC)
    where

import           Argon.SYB.Utils (Stage (..), everythingStaged)
import           Control.Arrow   ((&&&))
import           Data.Generics   (Data, mkQ)

import qualified GHC.Hs                     as GHC
import qualified GHC.Types.SrcLoc           as GHC
import qualified GHC.Types.Name.Reader      as GHC
import qualified GHC.Types.Name.Occurrence  as GHC

import           Argon.Loc
import           Argon.Types     (ComplexityBlock (..))

type Exp = GHC.HsExpr GHC.GhcPs
type Function = GHC.HsBind GHC.GhcPs
type MatchBody = GHC.LHsExpr GHC.GhcPs


-- | Compute cyclomatic complexity of every function binding in the given AST.
funcsCC :: (Data from) => from -> [ComplexityBlock]
funcsCC = map funCC . getBinds

funCC :: Function -> ComplexityBlock
funCC f = CC (getLocation $ GHC.fun_id f, getFuncName f, complexity f)

getBinds :: (Data from) => from -> [Function]
getBinds = everythingStaged Parser (++) [] $ mkQ [] visit
    where visit fun@GHC.FunBind {} = [fun]
          visit _                  = []

getLocation :: GHC.LIdP GHC.GhcPs -> Loc
getLocation = srcSpanToLoc . GHC.getLocA

getFuncName :: Function -> String
getFuncName = getName . GHC.unLoc . GHC.fun_id

complexity :: Function -> Int
complexity f = let matches = getMatches f
                   query = everythingStaged Parser (+) 0 $ 0 `mkQ` visit
                   visit = uncurry (+) . (visitExp &&& visitOp)
                in length matches + sumWith getGRHSsFromMatch matches + sumWith query matches

getMatches :: Function -> [GHC.LMatch GHC.GhcPs MatchBody]
getMatches = GHC.unLoc . GHC.mg_alts . GHC.fun_matches

getGRHSsFromMatch :: GHC.LMatch GHC.GhcPs MatchBody -> Int
getGRHSsFromMatch match = length (getGRHSs' match) - 1
  where
    getGRHSs' :: GHC.LMatch GHC.GhcPs MatchBody -> [GHC.LGRHS GHC.GhcPs MatchBody]
    getGRHSs' = GHC.grhssGRHSs . GHC.m_grhss . GHC.unLoc

getName :: GHC.RdrName -> String
getName = GHC.occNameString . GHC.rdrNameOcc

sumWith :: (a -> Int) -> [a] -> Int
sumWith f = sum . map f

visitExp :: Exp -> Int
visitExp GHC.HsIf {}            = 1
visitExp (GHC.HsMultiIf _ alts) = length alts - 1
visitExp (GHC.HsCase _ _ mg)    = length (GHC.unLoc . GHC.mg_alts $ mg) - 1
-- Since GHC 9.10 @\\case@/@\\cases@ are 'GHC.HsLam' tagged with a 'GHC.LamCase'
-- /'GHC.LamCases' variant; a plain @\\x -> e@ ('GHC.LamSingle') does not branch.
visitExp (GHC.HsLam _ GHC.LamCase mg)  = length (GHC.unLoc . GHC.mg_alts $ mg) - 1
visitExp (GHC.HsLam _ GHC.LamCases mg) = length (GHC.unLoc . GHC.mg_alts $ mg) - 1
visitExp _                      = 0

visitOp :: Exp -> Int
visitOp (GHC.OpApp _ _ (GHC.L _ (GHC.HsVar _ op)) _) =
    case getName (GHC.unLoc op) of
      "||" -> 1
      "&&" -> 1
      _    -> 0
visitOp _ = 0
