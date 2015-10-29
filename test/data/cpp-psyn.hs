{-# LANGUAGE CPP #-}
#ifndef HLINT
{-# LANGUAGE ViewPatterns #-}
#endif
{-# LANGUAGE PatternSynonyms #-}

data Counted a = Counted Int [a] deriving (Eq, Ord, Read, Show)

pattern (:+) :: () => () => a -> Counted a -> Counted a
pattern a :+ as <- Counted (subtract 1 -> i) (a : (Counted i -> as)) where
  a :+ Counted i as = Counted (i+1) (a:as)
