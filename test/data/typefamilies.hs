{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Lens.Lens
import Data.Functor.Identity
import Data.Functor.Product
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic (..), (:*:) (..), K1 (..), M1 (..), U1 (..))
import Control.Applicative

type family GSize (f :: * -> *)
type instance GSize U1 = Z
type instance GSize (K1 i c) = S Z
type instance GSize (M1 i c f) = GSize f
type instance GSize (a :*: b) = Add (GSize a) (GSize b)
