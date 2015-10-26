{-# LANGUAGE CPP #-}
#if 0
f = 3
#else
f m n = case 2*n of
          3 -> if m == 4 || m - n < 0 then 32 else 42
          _ -> 41
#endif
