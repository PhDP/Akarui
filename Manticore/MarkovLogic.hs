-- | Types and algorithms for Markov logic networks. The module has quite a
-- few 'fromStrings' methods that take strings and parse them into data
-- structure to make it easier to play with Markov logic in the repl.
module Manticore.MarkovLogic where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')
import Manticore.FOL
import qualified Manticore.Formula as F
import Manticore.Formula (Formula (..))
import Manticore.Predicate
import Manticore.Term
import Manticore.Parser
import Manticore.Symbols
import Manticore.Network
import qualified Manticore.KB as KB
import Manticore.KB (KB)

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
tellS :: String -> Double -> MLN String -> MLN String
tellS s w mln = case parseFOL s of
  Left _  -> mln
  Right f -> Map.insert f w mln

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
-- queries from the console. See 'Manticore.Parser.parseCondQuery' and
-- 'Manticore.Parser.parseJointQuery' to understand what kind of strings can
-- be parsed.
ask
  :: MLN String -- ^ A Markov logic network.
  -> [String] -- ^ A list of constants to ground the Markov logic network.
  -> String -- ^ A query to be parsed by 'Manticore.Parser.parseCondQuery' or 'Manticore.Parser.parseJointQuery'.
  -> Maybe Double -- ^ Either a double in [0.0, 1.0] or Nothing if the parsers fail.
ask mln terms query =
  case pq of
    Left _       ->
      case pj of
        Left _ -> Nothing
        Right q -> Just $ joint Map.empty mln ts q
    Right (q, c) -> Just $ conditional Map.empty mln ts q c
  where
    ts = map Constant terms
    pq = parseCondQuery query
    pj = parseJointQuery query

-- | Direct method of computing joint probabilities for Markov logic (does not
-- scale!).
joint
  :: Map (String, [Term String]) (Term String) -- ^ Resolve functions in predicates. If the predicates have no functions in them, provide Data.Map.empty.
  -> MLN String -- ^ The Markov logic network.
  -> [Term String] -- ^ List of constants to ground the Markov logic network.
  -> Map (Predicate String) Bool -- ^ An set of assignments.
  -> Double -- ^ A probability in [0.0, 1.0]
joint m mln ts query = sum (map evalNet toEval) / z
  where
    -- All possible assignments
    allass = allAss m ts fs
    -- Assignments to evaluate:
    toEval = filter valid allass
    -- Check if an assignment fits the query:
    valid ass = Map.foldrWithKey (\k v acc -> acc && case Map.lookup k ass of Just b -> v == b; _ -> False) True query
    -- The formula (the factors) to evaluate
    fs = allWGroundings m ts mln
    -- Value of the network for a given assignment.
    evalNet ass' = exp $ Map.foldrWithKey (\f w a -> val f w ass' + a) 0.0 fs
    -- Values of a factor
    val f w ass' = let v = F.eval ass' f in case v of
      Top    -> w
      Bottom -> 0.0
      _      -> error ("Eval failed for " ++ show v ++ " given " ++ show ass')
    -- The normalization factor
    z = foldl' (\a ass' -> evalNet ass' + a) 0.0 allass

-- | Direct method of computing marginal probabilities for Markov logic (does
-- not scale!).
marginal
  :: Map (String, [Term String]) (Term String) -- ^ Resolve functions in predicates. If the predicates have no functions in them, provide Data.Map.empty.
  -> MLN String -- ^ The Markov logic network.
  -> [Term String] -- ^ List of constants to ground the Markov logic network.
  -> Predicate String -- ^ An assignment to all predicates in the Markov logic network.
  -> Bool -- ^ Truth value of the predicate.
  -> Double -- ^ A probability in [0.0, 1.0]
marginal m mln ts p b = joint m mln ts $ Map.fromList [(p, b)]

-- | Direct method of computing conditional probabilities for Markov logic (does
-- not scale!).
conditional
  :: Map (String, [Term String]) (Term String) -- ^ Resolve functions in predicates. If the predicates have no functions in them, provide Data.Map.empty.
  -> MLN String -- ^ The Markov logic network.
  -> [Term String] -- ^ List of constants to ground the Markov logic network.
  -> Map (Predicate String) Bool -- ^ An set of assignments for the query.
  -> Map (Predicate String) Bool -- ^ Conditions.
  -> Double -- ^ A probability in [0.0, 1.0]
conditional m mln ts query cond = joint' (Map.union query cond) / joint' cond
  where joint' = joint m mln ts

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

-- | Algorithm to construct a network for Markov logic network inference. This
-- helper takes strings to make it easier to use in the console.
constructNetworkFromStrings :: String -> [String] -> [String] -> UNetwork (Predicate String)
constructNetworkFromStrings query ts mln = constructNetwork q e t m
  where
    (q, e) = case parseCondQuery query of
      Left _ -> (Set.empty, [])
      Right (q', e') -> (Map.keysSet q', Set.toList $ Map.keysSet e')
    t = map Constant ts
    m = fromStrings mln

-- | Builds a weighted knowledge base from a list of strings. If the parser
-- fails to parse a formula, it is ignored.
fromStrings :: [String] -> MLN String
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
