module Argon.Span (Span, srcSpanToSpan, spanToString, tagMsg)
    where

import Text.Printf (printf)
import Control.Arrow ((&&&))

import qualified SrcLoc     as GHC
import qualified FastString as GHC

type Span = (Int, Int, Int, Int)


srcSpanToSpan :: GHC.SrcSpan -> Span
srcSpanToSpan ss = (sl, sc, el, ec)
    where (sl, sc) = lloc $ GHC.srcSpanStart ss
          (el, ec) = lloc $ GHC.srcSpanEnd ss
          lloc = (GHC.srcLocLine &&& GHC.srcLocCol) . toRealSrcLoc
          toRealSrcLoc (GHC.RealSrcLoc z) = z
          toRealSrcLoc _ = GHC.mkRealSrcLoc (GHC.mkFastString "no info") 1 1

spanToString :: Span -> String
spanToString (l, c, _, _) = printf "%d:%d" l c

tagMsg :: Span -> String -> String
tagMsg s msg = spanToString s ++ " " ++ msg
