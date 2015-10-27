module Argon.Loc (Loc, srcSpanToLoc, locToString, tagMsg)
    where

import Text.Printf (printf)
import Control.Arrow ((&&&))

import qualified SrcLoc     as GHC
import qualified FastString as GHC

-- | Type synonym representing a location in the source code. The tuple
--   represents the following: @(start line, start col)@.
type Loc = (Int, Int)


srcSpanToLoc :: GHC.SrcSpan -> Loc
srcSpanToLoc ss = lloc $ GHC.srcSpanStart ss
    where lloc = (GHC.srcLocLine &&& GHC.srcLocCol) . toRealSrcLoc
          toRealSrcLoc (GHC.RealSrcLoc z) = z
          toRealSrcLoc _ = GHC.mkRealSrcLoc (GHC.mkFastString "no info") 0 0

locToString :: Loc -> String
locToString (l, c) = printf "%d:%d" l c

tagMsg :: Loc -> String -> String
tagMsg s msg = locToString s ++ " " ++ msg
