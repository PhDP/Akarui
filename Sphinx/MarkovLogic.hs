-- | Types and algorithms for Markov logic networks. The module has quite a
-- few 'fromStrings' methods that take strings and parse them into data
-- structure to make it easier to play with Markov logic in the repl.
--
-- For Markov logic, data is often represented with a Set (Predicate a, Bool).
-- This is prefered to Map since it simplifies queries such as
-- "P(Cancer(Bob) | !Cancer(Bob))", where a map would not allow these two
-- different predicate -> value mappings.
module Sphinx.MarkovLogic where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (partition)
import Control.Applicative ((<|>))
import Sphinx.FOL
import qualified Sphinx.Formula as F
import Sphinx.Formula (Formula (..))
import Sphinx.Predicate
import Sphinx.Term
import Sphinx.Parser
import Sphinx.Symbols
import Sphinx.Network
import qualified Sphinx.KB as KB
import Sphinx.KB (KB)

-- | A Markov logic network is a set of first-order logical formulas associated
-- with a weight.
type MLN t = Map (FOL t) Double

-- | Prints a Markov logic network.
fmtMLN :: (Show t) => MLN t -> String
fmtMLN = Map.foldrWithKey (\k v acc -> fmtWFormula symbolic k v ++ "\n" ++ acc) ""

-- | Prints a weighted formula.
fmtWFormula :: (Show t) => Symbols -> FOL t -> Double -> String
fmtWFormula s f w = showW ++ replicate nSpaces ' ' ++ F.prettyPrintFm s f
  where
    showW = show w
    nSpaces = 24 - length showW

-- | Adds a formula to the markov logic network using the parser. If the parser
-- fails, the function returns the MLN unmodified.
tell :: String -> MLN String -> MLN String
tell s mln = case parseWFOL s of
  Left _        -> mln
  Right (f, w)  -> Map.insert f w mln

-- | Gathers all the predicates of a markov logic network in a set.
allPredicates :: (Ord t) => MLN t -> Set (Predicate t)
allPredicates = Map.foldWithKey (\k _ acc -> Set.union (F.atoms k) acc) Set.empty

-- | Get all groundings from a Markov logic network.
allGroundings :: Map (String, [Term String]) (Term String) -> [Term String] -> MLN String -> KB (Predicate String)
allGroundings m ts mln = KB.allGroundings m ts (toKB mln)

-- | Get all groundings from a Markov logic network, keeping the weights
-- assigned to the original formula in the Markov logic network.
allWGroundings :: Map (String, [Term String]) (Term String) -> [Term String] -> MLN String -> MLN String
allWGroundings m ts =
  Map.foldrWithKey (\k v a -> Set.foldr' (\k' a' -> Map.insert k' v a') a (groundings m ts k)) Map.empty

-- | Builds a ground network for Markov logic.
groundNetwork :: Map (String, [Term String]) (Term String) -> [Term String] -> MLN String -> UNetwork (Predicate String)
groundNetwork m ts mln = Set.foldr' (\p acc -> Map.insert p (mb p) acc) Map.empty ps
  where
    -- All groundings from all formulas in the knowledge base:
    gs = Set.foldr' (\g acc -> Set.union (groundings m ts g) acc) Set.empty (Map.keysSet mln)
    -- All the predicates
    ps = KB.allPredicates gs
    -- The Markov blanket of predicate 'p', that is: all its neighbours.
    mb p = Set.delete p $ KB.allPredicates $ Set.filter (hasPred p) gs

-- | Returns all the factors in the MLN. Instead of mappings sets of predicates
-- to weights, this function maps them to the formula (the MLN provides the
-- weight).
factors :: Map (String, [Term String]) (Term String) -> [Term String] -> MLN String -> Map (Set (Predicate String)) (FOL String)
factors m ts mln = fs
  where
    -- All groundings mapped to their original formula
    gs = Set.foldr' (\k a -> Set.foldr' (`Map.insert` k) a (groundings m ts k)) Map.empty (Map.keysSet mln)
    -- Separate the formula in sets of predicates:
    fs = Map.foldrWithKey (\k v a -> Map.insert (F.atoms k) v a) Map.empty gs

-- | All possible assignments to the predicates in the network.
allAss ::
  Map (String, [Term String]) (Term String) ->
  [Term String] ->
  MLN String ->
  [Map (Predicate String) Bool]
allAss m ts mln = F.allAss $ allGroundings m ts mln

-- | Helper function to facilitate answering conditional & joint probability
-- queries from the console. See 'Sphinx.Parser.parseCondQuery' and
-- 'Sphinx.Parser.parseJointQuery' to understand what kind of strings can
-- be parsed.
ask
  :: MLN String -- ^ A Markov logic network.
  -> [String] -- ^ A list of constants to ground the Markov logic network.
  -> String -- ^ A query to be parsed by 'Sphinx.Parser.parseCondQuery' or 'Sphinx.Parser.parseJointQuery'.
  -> Maybe Double -- ^ Either a double in [0.0, 1.0] or Nothing if the parsers fail.
ask mln terms query = pq <|> pj
  where
    ts = map Constant terms
    pq = case parseCondQuery query of
      Left _ -> Nothing; Right (q, c) -> Just $ conditional Map.empty mln ts q c
    pj = case parseJointQuery query of
      Left _ -> Nothing; Right q -> Just $ joint Map.empty mln ts q

-- | Direct method of computing joint probabilities for Markov logic (does not
-- scale!).
partitionAss
  :: Set (Predicate String, Bool) -- ^ The joint query.
  -> [Map (Predicate String) Bool] -- ^ All possiblement assignments.
  -> ([Map (Predicate String) Bool], [Map (Predicate String) Bool]) -- ^ A probability in [0.0, 1.0]
partitionAss query = partition valid
  where
    -- Check if an assignment fits the query:
    valid ass = Set.foldr' (\(k, v) acc -> acc && case Map.lookup k ass of Just b -> v == b; _ -> False) True query

-- | Direct method of computing joint probabilities for Markov logic (does not
-- scale!).
joint
  :: Map (String, [Term String]) (Term String) -- ^ Resolve functions in predicates. If the predicates have no functions in them, provide Data.Map.empty.
  -> MLN String -- ^ The Markov logic network.
  -> [Term String] -- ^ List of constants to ground the Markov logic network.
  -> Set (Predicate String, Bool) -- ^ An set of assignments. The reason...
  -> Double -- ^ A probability in [0.0, 1.0]
joint m mln ts query = vq / z
  where
    vq = sum $ map evalNet toEval
    vo = sum $ map evalNet others
    z = vq + vo
    -- All possible assignments
    allass = allAss m ts fs
    -- Assignments to evaluate:
    (toEval, others) = partitionAss query allass
    -- The formula (the factors) to evaluate
    fs = allWGroundings m ts mln
    -- Value of the network for a given assignment.
    evalNet ass' = exp $ Map.foldrWithKey (\f w a -> val f w ass' + a) 0.0 fs
    -- Values of a factor
    val f w ass' = let v = F.eval ass' f in case v of
      Top    -> w
      Bottom -> 0.0
      _      -> error ("Eval failed for " ++ show v ++ " given " ++ show ass')

-- | Direct method of computing marginal probabilities for Markov logic (does
-- not scale!).
marginal
  :: Map (String, [Term String]) (Term String) -- ^ Resolve functions in predicates. If the predicates have no functions in them, provide Data.Map.empty.
  -> MLN String -- ^ The Markov logic network.
  -> [Term String] -- ^ List of constants to ground the Markov logic network.
  -> Predicate String -- ^ An assignment to all predicates in the Markov logic network.
  -> Bool -- ^ Truth value of the predicate.
  -> Double -- ^ A probability in [0.0, 1.0]Alicia Malone
marginal m mln ts p b = joint m mln ts $ Set.fromList [(p, b)]

-- | Direct method of computing conditional probabilities for Markov logic (does
-- not scale!).
conditional
  :: Map (String, [Term String]) (Term String) -- ^ Resolve functions in predicates. If the predicates have no functions in them, provide Data.Map.empty.
  -> MLN String -- ^ The Markov logic network.
  -> [Term String] -- ^ List of constants to ground the Markov logic network.
  -> Set (Predicate String, Bool) -- ^ An set of assignments for the query.
  -> Set (Predicate String, Bool) -- ^ Conditions.
  -> Double -- ^ A probability in [0.0, 1.0]
conditional m mln ts query cond = vnum / vden
   where
     vnum = sum $ map evalNet numerator
     vden = sum $ map evalNet denom
     -- All possible assignments
     allass = allAss m ts fs
     -- Assignments to evaluate:
     (numerator, _) = partitionAss (Set.union query cond) allass
     (denom, _ ) = partitionAss cond allass
     -- The formula (the factors) to evaluate
     fs = allWGroundings m ts mln
     -- Value of the network for a given assignment.
     evalNet ass' = exp $ Map.foldrWithKey (\f w a -> val f w ass' + a) 0.0 fs
     -- Values of a factor
     val f w ass' = let v = F.eval ass' f in case v of
       Top    -> w
       Bottom -> 0.0
       _      -> error ("Eval failed for " ++ show v ++ " given " ++ show ass')

-- | Algorithm to construct a network for Markov logic network inference.
--
-- Reference:
--   P Domingos and D Lowd, Markov Logic: An Interface Layer for Artificial
-- Intelligence, 2009, Morgan & Claypool. p. 26.
constructNetwork :: Set (Predicate String) -> [Predicate String] -> [Term String] -> MLN String -> UNetwork (Predicate String)
constructNetwork query evidence ts mln = Set.foldr' (\p acc -> Map.insert p (mb p) acc) Map.empty ps
  where
    -- All groundings from all formulas in the knowledge base:
    gs = Set.foldr' (\g acc -> Set.union (groundings Map.empty ts g) acc) Set.empty (Map.keysSet mln)
    -- Predicates in the network
    ps = step query query
    -- The Markov blanket of predicate 'p', that is: all its neighbours.
    mb p = Set.delete p $ KB.allPredicates $ Set.filter (hasPred p) gs
    -- One step of the algorithm
    step f g
      | Set.null f = g
      | Set.findMin f `elem` evidence = step (Set.deleteMin f) g
      | otherwise =
        let mbq = mb $ Set.findMin f in
        step
          (Set.union (Set.deleteMin f) (Set.intersection mbq g))
          (Set.union g mbq)

-- | Builds a weighted knowledge base from a list of strings. If
-- 'Sphinx.Parser.parseWFOL' fails to parse a formula, it is ignored.
fromStrings
  :: [String]   -- ^ A set of string, each of which is a first-order logic formula with a weight. Is being parsed by 'Sphinx.Parser.parseWFOL'.
  -> MLN String -- ^ A Markov logic network.
fromStrings = foldr
  (\k acc ->
    case parseWFOL k of
      Left _        -> acc
      Right (f, w)  -> Map.insert f w acc)
  Map.empty

-- | Converts a markov logic network to an unweighted knowledge base.
toKB :: (Ord t) => MLN t -> KB (Predicate t)
toKB = Map.keysSet

-- | Builds a Markov logic network with a first-order logic knowledge base
-- and a function mapping formulas to weights.
fromKB :: (Ord t) => (Formula (Predicate t) -> Double) -> KB (Predicate t) -> MLN t
fromKB w = Set.foldl' (\acc x -> Map.insert x (w x) acc) Map.empty
