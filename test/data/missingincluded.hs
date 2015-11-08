{-# LANGUAGE CPP #-}
#include "necessaryInclude.h"

#ifdef FOO
f n = case n of
        2 -> 2424
        3 -> 2
        _ -> 24241
#else
g = 42
#endif
