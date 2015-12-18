-- | A generic formula used for various logics, most notably propositional logic
-- and first-order logic (Sphinx.FOL module). The structure mostly follows
-- Harrison (2009), however, binary connectives ('and', 'or', ...) are
-- aggregated into a BinOp type.
--
-- Reference:
--   John Harrison, Handbook of Practical Logic and Automated Reasoning.
-- Cambridge University Press, 2009.
module Faun.Formula where

import System.Random
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (nub)
import Data.Monoid ((<>))
import Faun.Symbols
import qualified Faun.Text as FT
import qualified Data.Text as T
import Faun.BinT
import Faun.QualT
import Faun.ShowTxt

-- | A formula with generic atoms. Propositional logic can easily be described
-- with Formula String, and first-order logic is defined in module Faun.FOL as
-- Formula (Predicate t).
data Formula a =
  -- | Generic atoms.
    Atom a
  -- | The unary negation type.
  | Not (Formula a)
  -- | Binary connectives.
  | BinOp BinT (Formula a) (Formula a)
  -- | Qualifier apply to a string (following Harrison 2009).
  | Qualifier QualT T.Text (Formula a) -- Following Harris' here, but it might be smarter to put qualifiers in FOL only.

instance ShowTxt a => Show (Formula a) where
  show = T.unpack . prettyPrintFm symbolic

instance ShowTxt a => ShowTxt (Formula a) where
  showTxt = prettyPrintFm symbolic

instance Eq a => Eq (Formula a) where
  Atom a0 == Atom a1  = a0 == a1
  Not x0 == Not x1    = x0 == x1
  BinOp b0 x0 y0 == BinOp b1 x1 y1 =
    b0 == b1 && x0 == x1 && y0 == y1
  Qualifier q0 v0 x0 == Qualifier q1 v1 x1 =
    q0 == q1 && v0 == v1 && x0 == x1
  _ == _              = False

instance Ord a => Ord (Formula a) where
  Atom a0 `compare` Atom a1 = a0 `compare` a1
  Atom _ `compare` _ = GT
  _ `compare` Atom _ = LT
  Not f0 `compare` Not f1 = f0 `compare` f1
  Not _ `compare` _ = GT
  _ `compare` Not _ = LT
  BinOp b0 f00 f01 `compare` BinOp b1 f10 f11 =
    (b0 `compare` b1) <> (f00 `compare` f10) <> (f01 `compare` f11)
  BinOp{} `compare` Qualifier{} = GT
  Qualifier{} `compare` BinOp{} = LT
  Qualifier q0 v0 f0 `compare` Qualifier q1 v1 f1 =
    (q0 `compare` q1) <> (v0 `compare` v1) <> (f1 `compare` f0)

-- | Prints the formula given a set of symbols ('Sphinx.Symbols.Symbols').
-- This function is built to support printing in symbolic, LaTeX, and ASCII
-- formats.
prettyPrintFm :: (ShowTxt a) => Symbols -> Formula a -> T.Text
prettyPrintFm s = FT.rmQuotes . buildStr (0 :: Int)
  where
    -- For negation and qualifiers, add spaces after words but not symbols:
    notSpace = if T.toLower (symNot s) == "not" then " " else ""
    qualSpace = if T.toLower (symForall s) == "forall" then " " else ""
    suffixNot = symNot s == "'"

    -- Format prefixes:
    showNot b pr sym p =
      FT.surrIf b $ T.concat $ if suffixNot then [txt, sym] else [sym, notSpace, txt]
      where txt = buildStr (pr + 1) p

    -- Format infix operators:
    showInfix b pr sym p q =
      FT.surrIf b $ T.concat [buildStr (pr + 1) p, " ", sym, " ", buildStr pr q]

    -- Recursive function to build the string:
    buildStr pr fm = case fm of
      Atom a                -> showTxt a
      Not x                 -> showNot (pr > 12) 11 (symNot s) x
      BinOp And x y         -> showInfix (pr > 10) 10 (symAnd s) x y
      BinOp Or x y          -> showInfix (pr > 8) 8 (symOr s) x y
      BinOp Implies x y     -> showInfix (pr > 6) 6 (symImplies s) x y
      BinOp Xor x y         -> showInfix (pr > 4) 4 (symXor s) x y
      BinOp Iff x y         -> showInfix (pr > 2) 2 (symIff s) x y
      Qualifier ForAll v x  -> T.concat [symForall s, qualSpace, v, " ", buildStr pr x]
      Qualifier Exists v x  -> T.concat [symExists s, qualSpace, v, " ", buildStr pr x]
      Qualifier Unique v x  -> T.concat [symExists s, "!", qualSpace, v, " ", buildStr pr x]

-- | Count the number of atoms (Top & Bottom are considered atoms).
numAtoms :: Formula a -> Int
numAtoms f = case f of
  Not x           -> numAtoms x
  BinOp _ x y     -> numAtoms x + numAtoms y
  Qualifier _ _ x -> numAtoms x
  _               -> 1

-- | Gathers all atoms in the formula.
atoms :: (Ord a) => Formula a -> Set a
atoms = gat Set.empty
  where
    gat s fm = case fm of
      Atom z          -> Set.insert z s
      Not x           -> Set.union (atoms x) s
      BinOp _ x y     -> Set.unions [atoms x, atoms y, s]
      Qualifier _ _ x -> Set.union (atoms x) s

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

-- | Returns true if the formula has qualifiers
hasQual :: Formula a -> Bool
hasQual f = case f of
  Not x       -> hasQual x
  BinOp _ x y -> hasQual x || hasQual y
  Qualifier{} -> True
  _           -> False

-- | Gathers the variables inside some type of qualifier.
qualVars :: QualT -> Formula a -> Set T.Text
qualVars q = gat'
  where
    gat' = gat Set.empty
    gat s f' = case f' of
      Not x            -> Set.union s (gat' x)
      BinOp _ x y      -> Set.unions [s, gat' x, gat' y]
      Qualifier q' v x  ->
        if q == q' then Set.union (Set.insert v s) (gat' x)
        else Set.union s (gat' x)
      _                -> Set.empty

-- | Returns existentially qualified variables.
exiQualVars :: Formula a -> Set T.Text
exiQualVars = qualVars Exists

-- | Returns universally qualified variables.
uniQualVars :: Formula a -> Set T.Text
uniQualVars = qualVars ForAll

-- | Randomly assigns all element of the set to either True or False with equal
-- probability. It's a fair ass.
randomFairAss :: (Ord a) => StdGen -> Set a -> Map a Bool
randomFairAss g s = Map.fromList $ zip (Set.toList s) rs
 where rs = take (Set.size s) $ randoms g :: [Bool]

-- | Gathers and assigns all atoms to a boolean given a seed value.
randomFairAssF :: (Ord a) => StdGen -> Formula a -> Map a Bool
randomFairAssF g f = randomFairAss g $ atoms f

-- | Removes implications, equivalences, and exclusive disjunctions.
coreOp :: Formula a -> Formula a
coreOp f = case f of
  Not x             -> Not $ coreOp x
  BinOp And x y     -> BinOp And (coreOp x) (coreOp y)
  BinOp Or x y      -> BinOp Or (coreOp x) (coreOp y)
  BinOp Xor x y     -> BinOp Or (BinOp And (coreOp x) (Not $ coreOp y)) (BinOp And (Not $ coreOp x) (coreOp y))
  BinOp Implies x y -> BinOp Or (Not $ coreOp x) (coreOp y)
  BinOp Iff x y     -> BinOp Or (BinOp And (coreOp x) (coreOp y)) (BinOp And (Not $ coreOp x) (Not $ coreOp y))
  Qualifier q v x   -> Qualifier q v (coreOp x)
  _ -> f

-- | Normal form.
nnf :: Formula a -> Formula a
nnf f = case coreOp f of
  BinOp And x y       -> BinOp And (nnf x) (nnf y)
  BinOp Or x y        -> BinOp Or  (nnf x) (nnf y)
  Not (Not x)         -> nnf x
  Not (BinOp And x y) -> BinOp Or  (nnf (Not x)) (nnf (Not y))
  Not (BinOp Or x y)  -> BinOp And (nnf (Not x)) (nnf (Not y))
  _ -> f
