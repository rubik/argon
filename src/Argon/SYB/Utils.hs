-- The following code is temporarily taken from @alanz's fork of
-- nominolo/ghc-syb. Argon will use the original ghc-syb when a new version
-- is released on Hackage with @alanz's fixes.
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Argon.SYB.Utils (Stage(..), everythingStaged)
    where

import GHC
import NameSet (NameSet)
import Data.Generics
#if __GLASGOW_HASKELL__ <= 708
import Coercion
#endif


-- | Ghc Ast types tend to have undefined holes, to be filled
--   by later compiler phases. We tag Asts with their source,
--   so that we can avoid such holes based on who generated the Asts.
data Stage = Parser | Renamer | TypeChecker deriving (Eq, Ord, Show)

-- | Like 'everything', but avoid known potholes, based on the 'Stage' that
--   generated the Ast.
everythingStaged :: Stage -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingStaged stage k z f x
  | (const False
#if __GLASGOW_HASKELL__ <= 708
      `extQ` postTcType
      `extQ` nameList
      `extQ` coercion
      `extQ` cmdTable
#endif
      `extQ` fixity `extQ` nameSet) x = z
  | otherwise = foldl k (f x) (gmapQ (everythingStaged stage k z f) x)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
#if __GLASGOW_HASKELL__ <= 708
        postTcType = const (stage < TypeChecker)               :: PostTcType -> Bool
        nameList   = const (stage < TypeChecker)               :: [Name] -> Bool
        coercion   = const (stage < TypeChecker)               :: Coercion -> Bool
        cmdTable   = const (stage < TypeChecker)               :: CmdSyntaxTable RdrName -> Bool
#endif
        fixity     = const (stage < Renamer)                   :: GHC.Fixity -> Bool
