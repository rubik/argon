{-# LANGUAGE TemplateHaskell #-}
module Blow where

import Language.Haskell.TH

foo :: Q Exp
foo = [| \f -> f 2 |]
