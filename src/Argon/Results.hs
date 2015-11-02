{-# LANGUAGE OverloadedStrings #-}
module Argon.Results (order, filterResults, filterNulls, exportStream)
    where

import Data.Ord (comparing)
import Data.List (sortBy)
import Data.String (IsString)
import qualified Data.ByteString.Lazy as B

import Data.Aeson (encode)
import Pipes
import Pipes.Group
import qualified Pipes.Prelude as P
import Lens.Simple ((^.))

import Argon.Formatters
import Argon.Types


-- sortOn is built-in only in base 4.8.0.0 onwards
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

-- | Order a list of blocks. Ordering is done with respect to:
--
--     1. complexity (descending)
--     2. line number (ascending)
--     3. function name (alphabetically)
order :: [ComplexityBlock] -> [ComplexityBlock]
order = sortOn (\(CC ((l, _), f, cc)) -> (-cc, l, f))

filterNulls :: (FilePath, AnalysisResult) -> Bool
filterNulls (_, r) = case r of
                       Left  _  -> True
                       Right [] -> False
                       _        -> True

-- | Filter the results of the analysis, with respect to the given
--   'Config'.
filterResults :: Config
              -> (FilePath, AnalysisResult)
              -> (FilePath, AnalysisResult)
filterResults _ (s, Left err) = (s, Left err)
filterResults o (s, Right rs) =
    (s, Right $ order [r | r@(CC (_, _, cc)) <- rs, cc >= minCC o])

-- | Export analysis' results. How to export the data is defined by the
--   'Config' parameter.
exportStream :: Config
             -> Producer (FilePath, AnalysisResult) IO ()
             -> Effect IO ()
exportStream conf source =
    case outputMode conf of
      BareText -> source >-> P.map bareTextFormatter >-> P.mapM_ putStrLn
      Colored  -> source >-> P.map coloredTextFormatter >-> P.mapM_ putStrLn
      JSON     -> jsonStream (source >-> P.map encode) >-> P.mapM_ B.putStr

jsonStream :: IsString a
           => Producer a IO ()
           -> Producer a IO ()
jsonStream source = yield "[" *> intersperse' "," source <* yield "]\n"

intersperse' :: Monad m => a -> Producer a m r -> Producer a m r
intersperse' a producer = intercalates (yield a) (producer ^. chunksOf 1)
