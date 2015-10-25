module Argon.Span (Span, srcSpanToSpan, spanToString, tagMsg)
    where

import Text.Printf (printf)
import Control.Arrow ((&&&))
import qualified SrcLoc as GHC

type Span = (Int, Int, Int, Int)


srcSpanToSpan :: GHC.SrcSpan -> Span
srcSpanToSpan ss = (sl, sc, el, ec)
    where lloc = (GHC.srcLocLine &&& GHC.srcLocCol) . (\(GHC.RealSrcLoc z) -> z)
          (sl, sc) = lloc $ GHC.srcSpanStart ss -- TODO: Make it safe
          (el, ec) = lloc $ GHC.srcSpanEnd ss

spanToString :: Span -> String
spanToString (l, c, _, _) = printf "%d:%d" l c

tagMsg :: Span -> String -> String
tagMsg s msg = spanToString s ++ " " ++ msg
