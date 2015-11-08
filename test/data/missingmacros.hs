{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,5,0)
f a = if a == 0 then 2 else 3*a
#else
g = 0
#endif
