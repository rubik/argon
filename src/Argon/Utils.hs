module Argon.Utils (srcSpanToSpan, spanToString)
    where

import Text.Printf (printf)
import Control.Arrow ((&&&))
import qualified SrcLoc as GHC
import Argon.Types (Span)


srcSpanToSpan :: GHC.SrcSpan -> Span
srcSpanToSpan ss = (sl, sc, el, ec)
    where lloc = (GHC.srcLocLine &&& GHC.srcLocCol) . (\(GHC.RealSrcLoc z) -> z)
          (sl, sc) = lloc $ GHC.srcSpanStart ss -- TODO: Make it safe
          (el, ec) = lloc $ GHC.srcSpanEnd ss

spanToString :: Span -> String
spanToString (l, c, _, _) = printf "%d:%d" l c
