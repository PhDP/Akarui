-- | Types and algorithms for Markov logic networks.
module Sphinx.MLN where

import qualified Data.Set as Set
import Data.Set (Set)
import Sphinx.FOL
import Sphinx.Formula
import Sphinx.Parser
import Sphinx.Predicate

-- | A Markov logic network is a set of first-order logical formulas associated
-- with a weight.
type MLN t = [(FOL t, Double)]
-- type MLN t = Map (FOL t) Double

-- | Print MLN
showMLN :: (Show t) => MLN t -> String
showMLN mln = foldl1 (\a b -> a ++ "\n" ++ b) lines'
  where lines' = map (\f -> show (snd f) ++ "   " ++ show (fst f)) mln

-- | Adds a formula to the markov logic network with a string (and the parser).
tellS :: String -> Double -> MLN String -> MLN String
tellS s w mln = case parseFOL s of
  Left _  -> mln
  Right f -> (f, w) : mln

-- | Gathers all the predicates of a markov logic network in a set.
predicates :: (Ord t) => MLN t -> Set (Predicate t)
predicates mln = Set.unions $ map (atoms . fst) mln
