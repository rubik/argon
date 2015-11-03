{-# LANGUAGE Arrows #-}

-- | First argument: basis for a new "pretty" anchor if none exists yet
-- Second argument: a key ("ugly" anchor)
-- Returns: saved "pretty" anchor or created new one
getAnchor :: OdtReaderSafe (AnchorPrefix, Anchor) Anchor
getAnchor = proc (baseIdent, uglyAnchor) -> do
    state <- getExtraState -< ()
    returnA -< prettyAnchor
