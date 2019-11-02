{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Type and functions for first-order predicate logic.
module Akarui.FOL.FOL where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')
import qualified Data.Text as T
import Akarui.ShowTxt
import Akarui.FOL.Formula
import Akarui.FOL.Predicate (Predicate (Predicate))
import Akarui.FOL.Symbols (symbolic)
import qualified Akarui.FOL.Predicate as Pred
import Akarui.FOL.Term (Term (Constant, Variable, Function))
import qualified Akarui.FOL.Term as Term
import Akarui.FOL.PrettyPrint
import Akarui.FOL.BinT
import Akarui.FOL.QuanT

-- | A first-order logic formula is simply a formula of predicates.
type FOL = Formula Predicate

-- | Special "Truth", "Top", "True" predicate.
top :: FOL
top = Atom $ Predicate "Top" []

-- | Special "False", "Bot", "Bottom" predicate.
bot :: FOL
bot = Atom $ Predicate "Bot" []

instance Show FOL where
  show = T.unpack . prettyPrintFm symbolic

instance ShowTxt FOL where
  showTxt = prettyPrintFm symbolic

instance PrettyPrint FOL where
  prettyPrint = prettyPrintFm

-- | Extracts predicates from a list of formulas. If a formula is not an atom,
-- it will be ignored.
toPredicates :: [FOL] -> [Predicate]
toPredicates = foldl' (\acc f -> case f of Atom p -> p : acc; _ -> acc) []

-- | Tests if the formula is 'grounded', i.e. if it has no variables.
ground :: FOL -> Bool
ground f = case f of
  Atom (Predicate _ ts) -> all Term.ground ts
  BinOp _ x y           -> ground x || ground y
  Quantifier _ _ x      -> ground x
  _                     -> False

-- | Gathers all the variables in a first-order logic formula.
variables :: FOL -> Set T.Text
variables = gat Set.empty
  where
    -- Gathers variables from terms
    gatT s term = case term of
      Function _ ts -> foldl' gatT Set.empty ts
      Variable v    -> Set.insert v s
      Constant _    -> Set.empty
    -- Gathers variables from formula
    gat s fm = case fm of
      Atom (Predicate _ ts) -> foldl' gatT Set.empty ts
      Not x                 -> Set.union (gatE x) s
      BinOp _ x y           -> Set.unions [gatE x, gatE y, s]
      Quantifier _ _ x       -> Set.union (gatE x) s
    -- Gathers with an empty set
    gatE = gat Set.empty

-- | Test for the presence of a predicate in the formula.
hasPred :: Predicate -> FOL -> Bool
hasPred p f = case f of
  Atom p'         -> p == p'
  BinOp _ x y     -> hasPred p x || hasPred p y
  Quantifier _ _ x -> hasPred p x
  _               -> False

-- | Test for the presence of a predicate in the formula using only the name
-- of the predicate.
hasPredName :: FOL -> T.Text -> Bool
hasPredName f n = case f of
  Atom (Predicate n' _) -> n == n'
  BinOp _ x y     -> hasPredName x n || hasPredName y n
  Quantifier _ _ x -> hasPredName x n
  _               -> False

-- | Returns true if the formula has functions. This is often used in algorithms
-- where we must ensure all functions have been resolved to an object.
hasFun :: FOL -> Bool
hasFun f = case f of
  Atom (Predicate _ ts) -> any (\trm -> (Term.numFuns trm :: Int) > 0) ts
  BinOp _ x y           -> hasFun x || hasFun y
  Quantifier _ _ x       -> hasFun x
  _                     -> False

-- | Substitute a term in the formula.
substitute :: Term -> Term -> FOL -> FOL
substitute old new f = case f of
  Atom (Predicate n ts)
    -> Atom $ Predicate n $ map (Term.substitute old new) ts
  Not x           -> Not $ substitute old new x
  BinOp b x y     -> BinOp b (substitute old new x) (substitute old new y)
  Quantifier q v x -> Quantifier q v (substitute old new x)

-- | Shows the internal structure of the first-order logic formula. This is
-- mostly useful for testing and making sure the formula has the correct
-- structure.
showFOLStruct :: FOL -> T.Text
showFOLStruct f = case f of
  Atom a            -> Pred.showStruct a
  Not x             -> T.concat ["Not (", showFOLStruct x, ")"]
  BinOp b x y       -> T.concat [showTxt b, " (", showFOLStruct x, ") (", showFOLStruct y, ")"]
  Quantifier q v x   -> T.concat [showTxt q, " ", v, "(", showFOLStruct x, ")"]

-- | Resolves universal Quantifiers, substituting the variables in the 'ForAll'
-- for a given term (a constant, generally).
resolveForAll :: T.Text -> Term -> FOL -> FOL
resolveForAll v t f = case f of
  Not x                 -> Not $ resolveForAll v t x
  BinOp b x y           -> BinOp b (resolveForAll v t x) (resolveForAll v t y)
  Quantifier ForAll v' x  ->
    if v == v' then substitute (Variable v) t x
    else Quantifier ForAll v' (resolveForAll v t x)
  Quantifier Exists v' x  -> Quantifier Exists v' (resolveForAll v t x)
  _                     -> f

-- | Takes a formula, a map between functions and constants, and a list of
-- constants to produce a set of groundings.
--
-- Reference:
--   P Domingos and D Lowd, Markov Logic: An Interface Layer for Artificial
-- Intelligence, 2009, Morgan & Claypool. p. 14.
groundings :: [Term] -> FOL -> Set FOL
groundings cs f = loopV
  where
    groundSub v f' = case f' of
      Atom p ->
        if Pred.hasVar v p then
          let as = map (\c -> Atom $ Pred.substitute (Variable v) c p) cs in
          foldr1 (BinOp Or) as
        else
          f'
      Not x            -> Not $ groundSub v x
      BinOp b x y      -> BinOp b (groundSub v x) (groundSub v y)
      Quantifier q v' x -> Quantifier q v' (groundSub v' x)

    existsVar f' = case f' of
      Not x                 -> Not $ existsVar x
      BinOp b x y           -> BinOp b (existsVar x) (existsVar y)
      Quantifier Exists v x  -> existsVar $ groundSub v x
      Quantifier ForAll v x  -> Quantifier ForAll v $ existsVar x
      _                     -> f'

    f0 = existsVar f
    g0 = Set.fromList [f0]
    vs = uniquanVars f0

    loopV = Set.foldr' loopG g0 vs
    loopG v g = Set.foldr (\x a -> Set.union a (Set.fromList x)) Set.empty (gr v g)
      where
        gr v' =
          Set.map (\fm -> map (\c -> simplify $ resolveForAll v' c fm) cs)

-- | Returns all possible valuations of a set of formula.
allAss :: Set FOL -> [Map Predicate Bool]
allAss fs = if null as then [] else ms (head as) (tail as)
  where
    as = Set.toList $ Set.foldr Set.union Set.empty $ Set.map atoms fs
    ms atm s =
      if null s then
        [Map.fromList [(atm, True)], Map.fromList [(atm, False)]]
      else
        map (Map.insert atm True) (ms (head s) (tail s)) ++
         map (Map.insert atm False) (ms (head s) (tail s))

-- | The unary negation operator.
lneg :: FOL -> FOL
lneg (Not y) = y
lneg x
  | x == top                = bot
  | x == bot                = top
  | otherwise               = Not x

-- | The 'and' (conjunction) binary operator.
land :: FOL -> FOL -> FOL
land x y
  | x == top && y == top    = top
  | x == bot || y == bot    = bot
  | x == top                = y
  | y == top                = x
  | otherwise               = BinOp And x y

-- | The 'or' (inclusive disjunction) binary operator.
lor :: FOL -> FOL -> FOL
lor x y
  | x == top || y == top    = top
  | x == bot                = y
  | y == bot                = x
  | otherwise               = BinOp Or x y

-- | The 'exclusive or' (exclusive disjunction) binary operator.
lxor :: FOL -> FOL -> FOL
lxor x y
  | y == bot                = x
  | x == bot                = y
  | y == top                = lneg x
  | x == top                = lneg y
  | otherwise               = BinOp Xor x y

-- | The 'implies' (implication) binary operator.
limplies :: FOL -> FOL -> FOL
limplies x y
  | x == top                = y
  | x == bot                = top
  | y == bot                = lneg x
  | y == top                = top
  | otherwise               = BinOp Implies x y

-- | The 'if and only if' (equivalence) binary operator.
liff :: FOL -> FOL -> FOL
liff x y
  | x == top                = y
  | x == bot && y == bot    = top
  | x == bot                = lneg y
  | y == bot                = lneg x
  | y == top                = x
  | otherwise               = BinOp Iff x y

-- | Dispatch binary operators to their resolution function.
binOperator :: BinT -> FOL -> FOL -> FOL
binOperator b = case b of
  And     -> land
  Or      -> lor
  Xor     -> lxor
  Implies -> limplies
  Iff     -> liff

-- | Simplify using Harris' algorithm.
simplify :: FOL -> FOL
simplify f = case f of
  Not x             -> lneg $ sim1 $ simplify x
  BinOp b x y       -> binOperator b (sim1 $ simplify x) (sim1 $ simplify y)
  Quantifier q v x   -> Quantifier q v $ sim1 $ simplify x
  _                 -> f
  where
    sim1 f' = case f' of
      Not x           -> lneg $ sim1 x
      BinOp b x y     -> binOperator b (sim1 x) (sim1 y)
      Quantifier q v x -> Quantifier q v $ sim1 x
      _               -> f'

-- | Evaluates a formula given an assignment to atoms. If the assignment is
-- incomplete, eval with evaluate as much as possible but might not reduce
-- formula to top/bot. This function completely ignores Quantifiers. For
-- functions that rely on Quantifiers, see the Sphinx.FOL first-order logic
-- module.
eval :: Map Predicate Bool -> FOL -> FOL
eval ass = simplify . eval'
 where
   eval' f' = case f' of
     Atom a            -> case Map.lookup a ass of
       Just True   -> top
       Just False  -> bot
       Nothing     -> f'
     Not x             -> lneg $ eval' x
     BinOp b x y       -> BinOp b (eval' x) (eval' y)
     Quantifier _ _ x   -> eval' x

-- | Given an assignment to atoms, test whethers the formula evaluates to 'True'
-- This functions ignores Quantifiers (if present, and they should not be there).
satisfy :: Map Predicate Bool -> FOL -> Bool
satisfy ass f = eval ass f == top

-- | Given an assignment to atoms, test whethers the formula fails to evaluate
-- to true. That is: unsatisfiable means it evaluates to bot or failed to
-- evaluate to top/bot.
unsatisfiable :: Map Predicate Bool -> FOL -> Bool
unsatisfiable ass f = eval ass f /= top

-- | Takes a formula, a list of assignments, and returns how many were true,
-- false, or undefined (could not be reduced to either top or bot).
numTrueFalse :: (Integral n) => FOL -> [Map Predicate Bool] -> (n, n, n)
numTrueFalse f =
  foldl'
    (\(t, b, u) ass ->
      let v = eval ass f in
        if v == top then (t + 1, b, u)
        else if v == bot then (t, b + 1, u)
        else (t, b, u + 1))
    (0, 0, 0)
