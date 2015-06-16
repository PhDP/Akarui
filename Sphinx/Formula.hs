-- | A generic formula used for various logics, most notably propositional logic
-- and first-order logic (Sphinx.FOL module). The structure mostly follows
-- Harrison (2009), however, binary connectives ('and', 'or', ...) are
-- aggregated into a BinOp type.
--
-- References:
--   John Harrison, Handbook of Practical Logic and Automated Reasoning.
-- Cambridge University Press, 2009.
module Sphinx.Formula where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (nub)
import System.Random
import Sphinx.Symbols
import Sphinx.Text

-- | Supported binary connectives (in order of precedence).
data BinT =
  -- | Conjunction. Returns true only if both sides are true.
    And
  -- | Disjunction. Returns true if at least one operand is true.
  | Or
  -- | Implication is... messed up. It returns true except if the
  -- left operand is true and the right one is false, e.g. True implies False
  -- is the only situation where implication returns false.
  | Implies
  -- | Exclusive disjunction. Returns true if one and only one operand is true.
  | Xor
  -- | Equivalence. Returns true is both operand have the same value, i.e. both
  -- true or both are false.
  | Iff
  deriving (Eq, Ord, Show)

-- | Supported qualifiers. They are only used in some logics, for example they
-- make no sense in propositional logic.
data QualT =
  -- | Univeral qualifier.
    ForAll
  -- | Existential qualifier.
  | Exists
  deriving (Eq, Ord, Show)

-- | A formula with generic atoms. Propositional logic can easily be described
-- with Formula String, and first-order logic is defined in module Sphinx.FOL as
-- Formula (Predicate t).
data Formula a =
  -- | Another name for 'True' to avoid confusion with Prelude.True.
    Top
  -- | Another name for 'False'.
  | Bottom
  -- | Generic atoms.
  | Atom a
  -- | The unary negation type.
  | Not (Formula a)
  -- | Binary connectives.
  | BinOp BinT (Formula a) (Formula a)
  -- | Qualifier apply to a string (following Harrison 2009).
  | Qualifier QualT String (Formula a) -- Following Harris' here, but it might be smarter to put qualifiers in FOL only.

instance Show a => Show (Formula a) where
  show = showFm long -- symbolic

instance Eq a => Eq (Formula a) where
  f0 == f1 = f0 `sameAs` f1
    where
      sameAs a b = case (a, b) of
        (Top, Top)          -> True
        (Bottom, Bottom)    -> True
        (Atom a0, Atom a1)  -> a0 == a1
        (Not x0, Not x1)    -> x0 `sameAs` x1
        (BinOp b0 x0 y0, BinOp b1 x1 y1) ->
          b0 == b1 && x0 `sameAs` x1 && y0 `sameAs` y1
        (Qualifier q0 v0 x0, Qualifier q1 v1 x1) ->
          q0 == q1 && v0 == v1 && x0 `sameAs` x1
        _ -> False

-- instance Ord a => Ord (Formula a) where

-- | Prints the formula given a set of symbols ('Sphinx.Symbols.Symbols').
-- This function is built to support printing in symbolic, LaTeX, and ASCII
-- formats.
showFm :: (Show a) => Symbols -> Formula a -> String
showFm s = rmQuotes . buildStr (0 :: Int)
  where
    --notSpace = " "
    --qualSpace = " "

    -- Surrouds the strings if b is true:
    surr b str = if b then "(" ++ str ++ ")" else str

    -- Format prefixes:
    showPrefix b pr sym p = surr b (sym ++ buildStr (pr + 1) p)

    -- Format infix operators:
    showInfix b pr sym p q =
      surr b (buildStr (pr + 1) p ++ " " ++ sym ++ " " ++ buildStr pr q)

    -- Recursive function to build the string:
    buildStr pr fm = case fm of
      Atom a                -> show a
      Top                   -> symTop s
      Bottom                -> symBottom s
      Not x                 -> showPrefix (pr > 12) 11 (symNot s) x
      BinOp And x y         -> showInfix (pr > 10) 10 (symAnd s) x y
      BinOp Or x y          -> showInfix (pr > 8) 8 (symOr s) x y
      BinOp Implies x y     -> showInfix (pr > 6) 6 (symImplies s) x y
      BinOp Xor x y         -> showInfix (pr > 4) 4 (symXor s) x y
      BinOp Iff x y         -> showInfix (pr > 2) 2 (symIff s) x y
      Qualifier ForAll v x  -> symForall s ++ " " ++ v ++ " " ++ buildStr pr x
      Qualifier Exists v x  -> symExists s ++ " " ++ v ++ " " ++ buildStr pr x

-- | Gathers all atoms in the formula.
atoms :: (Ord a) => Formula a -> Set a
atoms = gat
  where
    gat = gat' Set.empty
    gat' s fm = case fm of
      Atom z          -> Set.insert z s
      Not x           -> Set.union (gat x) s
      BinOp _ x y     -> Set.unions [gat x, gat y, s]
      Qualifier _ _ x -> Set.union (gat x) s
      _               -> Set.empty

-- | Gathers all atoms in the formula in a list for atoms that do not support
-- the Ord type class.
atomsLs :: (Eq a) => Formula a -> [a]
atomsLs = nub . gat
  where
    gat = gat' []
    gat' l fm = case fm of
      Atom z          -> z : l
      Not x           -> l ++ gat x
      BinOp _ x y     -> l ++ gat x ++ gat y
      Qualifier _ _ x -> l ++ gat x
      _               -> []

-- | The unary negation operator.
lneg :: Formula a -> Formula a
lneg f = case f of
  Top -> Bottom
  Bottom -> Top
  Not x   -> x
  _       -> Not f

-- | he 'and' (conjunction) binary operator.
land :: Formula a -> Formula a -> Formula a
land f0 f1 = case (f0, f1) of
  (Top, Top)  -> Top
  (_, Bottom) -> Bottom
  (Bottom, _) -> Bottom
  (Top, y)    -> y
  (x, Top)    -> x
  (x, y)      -> BinOp And x y

-- | The 'or' (inclusive disjunction) binary operator.
lor :: Formula a -> Formula a -> Formula a
lor f0 f1 = case (f0, f1) of
  (Top, _)    -> Top
  (_, Top)    -> Top
  (Bottom, y) -> y
  (x, Bottom) -> x
  (x, y)      -> BinOp Or x y

-- | The 'exclusive or' (exclusive disjunction) binary operator.
lxor :: Formula a -> Formula a -> Formula a
lxor f0 f1 = case (f0, f1) of
  (x, Bottom) -> x
  (Bottom, y) -> y
  (x, Top)    -> lneg x
  (Top, y)    -> lneg y
  (x, y)      -> BinOp Xor x y

-- | The 'implies' (implication) binary operator.
limplies :: Formula a -> Formula a -> Formula a
limplies f0 f1 = case (f0, f1) of
  (Top, y)    -> y
  (Bottom, _) -> Top
  (x, Bottom) -> lneg x
  (_, Top)    -> Top
  (x, y)      -> BinOp Implies x y

-- | The 'if and only if' (equivalence) binary operator.
liff :: Formula a -> Formula a -> Formula a
liff f0 f1 = case (f0, f1) of
  (Top, y)         -> y
  (Bottom, Bottom) -> Top
  (Bottom, y)      -> lneg y
  (x, Bottom)      -> lneg x
  (x, Top)         -> x
  (x, y)           -> BinOp Iff x y

-- | Dispatch binary operators to their resolution function.
binOperator :: Formula a -> Formula a
binOperator b = case b of
  BinOp And x y     -> land x y
  BinOp Or x y      -> lor x y
  BinOp Xor x y     -> lxor x y
  BinOp Implies x y -> limplies x y
  BinOp Iff x y     -> liff x y
  _                 -> b

-- | Simplify using Harris' algorithm.
simplify :: Formula a -> Formula a
simplify f = case f of
  Not x             -> lneg $ sim1 $ simplify x
  BinOp And x y     -> land     (sim1 $ simplify x) (sim1 $ simplify y)
  BinOp Or x y      -> lor      (sim1 $ simplify x) (sim1 $ simplify y)
  BinOp Xor x y     -> lxor     (sim1 $ simplify x) (sim1 $ simplify y)
  BinOp Implies x y -> limplies (sim1 $ simplify x) (sim1 $ simplify y)
  BinOp Iff x y     -> liff     (sim1 $ simplify x) (sim1 $ simplify y)
  Qualifier q v x   -> Qualifier q v $ sim1 $ simplify x
  _                 -> f
  where
    sim1 f' = case f' of
      Not x             -> lneg $ sim1 x
      BinOp And x y     -> land     (sim1 x) (sim1 y)
      BinOp Or x y      -> lor      (sim1 x) (sim1 y)
      BinOp Xor x y     -> lxor     (sim1 x) (sim1 y)
      BinOp Implies x y -> limplies (sim1 x) (sim1 y)
      BinOp Iff x y     -> liff     (sim1 x) (sim1 y)
      Qualifier q v x   -> Qualifier q v $ sim1 x
      _                 -> f'

-- rmQualifiers

-- | Evaluates a formula given an assignment to atoms. If the assignment is
-- incomplete, eval with evaluate as much as possible but might not reduce
-- formula to Top/Bottom. This function completely ignores qualifiers. For
-- functions that rely on qualifiers, see the Sphinx.FOL first-order logic
-- module.
eval :: (Ord a) => Map a Bool -> Formula a -> Formula a
eval ass f = simplify $ eval' f
  where
    eval' f' = case f' of
      Atom a            -> case Map.lookup a ass of
        Just True   -> Top
        Just False  -> Bottom
        Nothing     -> f'
      Not x             -> lneg $ eval' x
      BinOp b x y       -> BinOp b (eval' x) (eval' y)
      Qualifier _ _ x   -> eval' x
      _                 -> f'

-- | Given an assignment to atoms, test whethers the formula evaluates to 'True'
-- This functions ignores qualifiers (if present, and they should not be there).
satisfiable :: (Ord a) => Map a Bool -> Formula a -> Bool
satisfiable ass f = case eval ass f of Top -> True; _ -> False

-- | Given an assignment to atoms, test whethers the formula fails to evaluate
-- to true. That is: unsatisfiable means it evaluates to Bottom or failed to
-- evaluate to Top/Bottom.
unsatisfiable :: (Ord a) => Map a Bool -> Formula a -> Bool
unsatisfiable ass f = case eval ass f of Top -> False; _ -> True

-- | Randomly assigns all element of the set to either True or False with equal
-- probability. It's a fair ass.
randomFairAss :: (Ord a) => Int -> Set a -> Map a Bool
randomFairAss seed s = Map.fromList $ zip (Set.toList s) rs
  where rs = take (Set.size s) $ randoms (mkStdGen seed) :: [Bool]

-- | Gathers and assigns all atoms to a boolean given a seed value.
randomFairAssF :: (Ord a) => Int -> Formula a -> Map a Bool
randomFairAssF seed f = randomFairAss seed $ atoms f
