{-# LANGUAGE MultiWayIf #-}
f n = if | n `mod` 34 == 0 -> 3
         | n `div` 24 == 1 -> 24
         | n + 42 - 4 == 0 -> 2424
         | _               -> 42
