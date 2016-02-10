module Argon.Visitor (funcsCC)
    where

import Data.Generics (Data, Typeable, mkQ)
import Argon.SYB.Utils (Stage(..), everythingStaged)
import Control.Arrow ((&&&))

import qualified GHC
import qualified RdrName as GHC
import qualified OccName as GHC

import Argon.Loc
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
getBinds = everythingStaged Parser (++) [] $ mkQ [] visit
    where visit fun@GHC.FunBind {} = [fun]
          visit _ = []

getLocation :: GHC.Located a -> Loc
getLocation = srcSpanToLoc . GHC.getLoc

getFuncName :: Function -> String
getFuncName = getName . GHC.unLoc . GHC.fun_id

complexity :: Function -> Int
complexity f = let matches = getMatches f
                   query = everythingStaged Parser (+) 0 $ 0 `mkQ` visit
                   visit = uncurry (+) . (visitExp &&& visitOp)
                in length matches + sumWith query matches

getMatches :: Function -> [GHC.LMatch GHC.RdrName MatchBody]
getMatches = GHC.mg_alts . GHC.fun_matches

getName :: GHC.RdrName -> String
getName = GHC.occNameString . GHC.rdrNameOcc

sumWith :: (a -> Int) -> [a] -> Int
sumWith f = sum . map f

visitExp :: Exp -> Int
visitExp GHC.HsIf {} = 1
visitExp (GHC.HsMultiIf _ alts) = length alts - 1
visitExp (GHC.HsLamCase _ alts) = length (GHC.mg_alts alts) - 1
visitExp (GHC.HsCase _ alts)    = length (GHC.mg_alts alts) - 1
visitExp _ = 0

visitOp :: Exp -> Int
visitOp (GHC.OpApp _ (GHC.L _ (GHC.HsVar op)) _ _) =
    case getName op of
      "||" -> 1
      "&&" -> 1
      _    -> 0
visitOp _ = 0
