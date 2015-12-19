-- | Types and algorithms for Markov logic networks. The module has quite a
-- few 'fromStrings' methods that take strings and parse them into data
-- structure to make it easier to play with Markov logic in the repl.
--
-- For Markov logic, data is often represented with a Set (Predicate a, Bool).
-- This is prefered to Map since it simplifies queries such as
-- "P(Cancer(Bob) | !Cancer(Bob))", where a map would not allow these two
-- different predicate -> value mappings.
module Faun.MarkovLogic
( MLN(..)
, tell
, allPredicates
, allGroundings
, allWGroundings
, fromStrings
, groundNetwork
, factors
, ask
, marginal
, joint
, conditional
, constructNetwork
) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (partition)
import Control.Applicative ((<|>))
import qualified Faun.FOL as FOL
import Faun.FOL (FOL)
import qualified Faun.Formula as F
import Faun.Predicate
import Faun.Term
import Faun.Symbols
import Faun.Network
import Faun.ShowTxt
import qualified Faun.FormulaSet as FS
import Faun.Parser.Probability
import Faun.Parser.FOL

-- | A Markov logic network is a set of first-order logical formulas associated
-- with a weight.

data MLN = MLN { network :: Map FOL Double }

instance Show MLN where
  show = T.unpack . fmtMLN

instance ShowTxt MLN where
  showTxt = fmtMLN

-- | Prints a Markov logic network.
fmtMLN :: MLN -> T.Text
fmtMLN (MLN m) =
  Map.foldrWithKey
    (\k v acc -> T.concat [fmtWFormula symbolic k v, "\n", acc]) "" m

-- | Prints a weighted formula.
fmtWFormula :: Symbols -> FOL -> Double -> T.Text
fmtWFormula s f w = T.concat [F.prettyPrintFm s f, ", ", T.pack $ show w, "."]

-- | Adds a formula to the markov logic network using the parser. If the parser
-- fails, the function returns the MLN unmodified.
tell :: String -> MLN -> MLN
tell s mln@(MLN m) = case parseWFOL s of
  Left _        -> mln
  Right (f, w)  -> MLN $ Map.insert f w m

-- | Gathers all the predicates of a markov logic network in a set.
allPredicates :: MLN -> Set Predicate
allPredicates (MLN m) =
  Map.foldWithKey (\k _ acc -> Set.union (F.atoms k) acc) Set.empty m

-- | Get all groundings from a Markov logic network.
allGroundings :: [Term] -> MLN -> Set FOL
allGroundings ts (MLN m) = FS.allGroundings ts $ Map.keysSet m

-- | Get all groundings from a Markov logic network, keeping the weights
-- assigned to the original formula in the Markov logic network.
allWGroundings :: [Term] -> MLN -> MLN
allWGroundings ts (MLN m) =
  MLN $ Map.foldrWithKey
    (\k v a -> Set.foldr' (\k' a' -> Map.insert k' v a') a (FOL.groundings ts k))
    Map.empty
    m

-- | Builds a ground network for Markov logic.
groundNetwork :: [Term] -> MLN -> UNetwork Predicate
groundNetwork ts (MLN m) = Set.foldr' (\p acc -> Map.insert p (mb p) acc) Map.empty ps
  where
    -- All groundings from all formulas in the knowledge base:
    gs = Set.foldr' (\g acc -> Set.union (FOL.groundings ts g) acc) Set.empty (Map.keysSet m)
    -- All the predicates
    ps = FS.allPredicates gs
    -- The Markov blanket of predicate 'p', that is: all its neighbours.
    mb p = Set.delete p $ FS.allPredicates $ Set.filter (FOL.hasPred p) gs

-- | Returns all the factors in the MLN. Instead of mappings sets of predicates
-- to weights, this function maps them to the formula (the MLN provides the
-- weight).
factors :: [Term] -> MLN -> Map (Set Predicate) FOL
factors ts (MLN m) = fs
  where
    -- All groundings mapped to their original formula
    gs = Set.foldr' (\k a -> Set.foldr' (`Map.insert` k) a (FOL.groundings ts k)) Map.empty (Map.keysSet m)
    -- Separate the formula in sets of predicates:
    fs = Map.foldrWithKey (\k v a -> Map.insert (F.atoms k) v a) Map.empty gs

-- | All possible assignments to the predicates in the network.
allAss ::
  [Term] ->
  MLN ->
  [Map Predicate Bool]
allAss ts mln = FOL.allAss $ allGroundings ts mln

-- | Helper function to facilitate answering conditional & joint probability
-- queries from the console. See 'Sphinx.Parser.parseCondQuery' and
-- 'Sphinx.Parser.parseJointQuery' to understand what kind of strings can
-- be parsed.
ask
  :: MLN -- ^ A Markov logic network.
  -> [T.Text] -- ^ A list of constants to ground the Markov logic network.
  -> String -- ^ A query to be parsed by 'Sphinx.Parser.parseCondQuery' or 'Sphinx.Parser.parseJointQuery'.
  -> Maybe Double -- ^ Either a double in [0.0, 1.0] or Nothing if the parsers fail.
ask mln terms query = pq <|> pj
  where
    ts = map Constant terms
    pq = case parseCondQuery query of
      Left _ -> Nothing; Right (q, c) -> Just $ conditional mln ts q c
    pj = case parseJointQuery query of
      Left _ -> Nothing; Right q -> Just $ joint mln ts q

-- | Direct method of computing joint probabilities for Markov logic (does not
-- scale!).
partitionAss
  :: Set (Predicate, Bool) -- ^ The joint query.
  -> [Map Predicate Bool] -- ^ All possiblement assignments.
  -> ([Map Predicate Bool], [Map Predicate Bool]) -- ^ A probability in [0.0, 1.0]
partitionAss query = partition valid
  where
    -- Check if an assignment fits the query:
    valid ass = Set.foldr' (\(k, v) acc -> acc && case Map.lookup k ass of Just b -> v == b; _ -> False) True query

-- | Direct method of computing joint probabilities for Markov logic (does not
-- scale!).
joint
  :: MLN -- ^ The Markov logic network.
  -> [Term] -- ^ List of constants to ground the Markov logic network.
  -> Set (Predicate, Bool) -- ^ An set of assignments. The reason...
  -> Double -- ^ A probability in [0.0, 1.0]
joint mln ts query = vq / z
  where
    vq = sum $ map evalNet toEval
    vo = sum $ map evalNet others
    z = vq + vo
    -- All possible assignments
    allass = allAss ts fs
    -- Assignments to evaluate:
    (toEval, others) = partitionAss query allass
    -- The formula (the factors) to evaluate
    fs = allWGroundings ts mln
    -- Value of the network for a given assignment.
    evalNet ass' = exp $ Map.foldrWithKey (\f w a -> val f w ass' + a) 0.0 (network fs)
    -- Values of a factor
    val f w ass' = let v = FOL.eval ass' f in
      if v == FOL.top then w
      else if v == FOL.bot then 0.0
      else error ("Eval failed for " ++ show v ++ " given " ++ show ass')

-- | Direct method of computing marginal probabilities for Markov logic (does
-- not scale!).
marginal
  :: MLN -- ^ The Markov logic network.
  -> [Term] -- ^ List of constants to ground the Markov logic network.
  -> Predicate-- ^ An assignment to all predicates in the Markov logic network.
  -> Bool -- ^ Truth value of the predicate.
  -> Double -- ^ A probability in [0.0, 1.0]Alicia Malone
marginal mln ts p b = joint mln ts $ Set.fromList [(p, b)]

-- | Direct method of computing conditional probabilities for Markov logic (does
-- not scale!).
conditional
  :: MLN -- ^ The Markov logic network.
  -> [Term] -- ^ List of constants to ground the Markov logic network.
  -> Set (Predicate, Bool) -- ^ An set of assignments for the query.
  -> Set (Predicate, Bool) -- ^ Conditions.
  -> Double -- ^ A probability in [0.0, 1.0]
conditional mln ts query cond = vnum / vden
   where
     vnum = sum $ map evalNet numerator
     vden = sum $ map evalNet denom
     -- All possible assignments
     allass = allAss ts fs
     -- Assignments to evaluate:
     (numerator, _) = partitionAss (Set.union query cond) allass
     (denom, _ ) = partitionAss cond allass
     -- The formula (the factors) to evaluate
     fs = allWGroundings ts mln
     -- Value of the network for a given assignment.
     evalNet ass' = exp $ Map.foldrWithKey (\f w a -> val f w ass' + a) 0.0 (network fs)
     -- Values of a factor
     val f w ass' = let v = FOL.eval ass' f in
       if v == FOL.top then w
       else if v == FOL.bot then 0.0
       else error ("Eval failed for " ++ show v ++ " given " ++ show ass')

-- | Algorithm to construct a network for Markov logic network inference.
--
-- Reference:
--   P Domingos and D Lowd, Markov Logic: An Interface Layer for Artificial
-- Intelligence, 2009, Morgan & Claypool. p. 26.
constructNetwork :: Set Predicate -> [Predicate] -> [Term] -> MLN -> UNetwork Predicate
constructNetwork query evidence ts (MLN m) = Set.foldr' (\p acc -> Map.insert p (mb p) acc) Map.empty ps
  where
    -- All groundings from all formulas in the knowledge base:
    gs = Set.foldr' (\g acc -> Set.union (FOL.groundings ts g) acc) Set.empty (Map.keysSet m)
    -- Predicates in the network
    ps = step query query
    -- The Markov blanket of predicate 'p', that is: all its neighbours.
    mb p = Set.delete p $ FS.allPredicates $ Set.filter (FOL.hasPred p) gs
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
  -> MLN -- ^ A Markov logic network.
fromStrings s = MLN $ foldr
  (\k acc ->
    case parseWFOL k of
      Left _        -> acc
      Right (f, w)  -> Map.insert f w acc)
  Map.empty
  s
