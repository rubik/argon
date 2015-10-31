module Internal.Sparse where

import Foreign.C.Types(CInt(..))
import Foreign(Ptr)

foreign import ccall unsafe "smXv"
  c_smXv :: SMxV

foreign import ccall unsafe "smTXv"
  c_smTXv :: SMxV
