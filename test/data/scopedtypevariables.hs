{-# LANGUAGE ScopedTypeVariables #-}

module Utility.Exception where

import Control.Monad.Catch as X hiding (Handler)
import qualified Control.Monad.Catch as M

catchNonAsync :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchNonAsync a onerr = a catches
 [ M.Handler ( (e :: SomeException) -> onerr e)
 ]
