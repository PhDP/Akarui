-- | Type and functions for predicates: the atoms of first-order logic.
module Faun.Predicate where

import qualified Faun.Text as FT
import Faun.Term (Term)
import qualified Faun.Term as Term
import qualified Data.Text as T
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Faun.ShowTxt

-- | Predicates are atoms (thus they evaluate to true/false) mapping a list
-- of terms (objects) to a truth value.
data Predicate =
  -- | Builds a predicate with a string and a list of terms.
  Predicate T.Text [Term]

-- | True (Top) predicate.
true :: Predicate
true = Predicate "True" []

-- | False (Bottom, Bot) predicate.
false :: Predicate
false = Predicate "False" []

instance Eq Predicate where
  (Predicate n0 ts0) == (Predicate n1 ts1) =
    n0 == n1 && length ts0 == length ts1 && all (uncurry (==)) (zip ts0 ts1)

instance Ord Predicate where
  (Predicate n0 ts0) `compare` (Predicate n1 ts1) = Term.compareFun n0 ts0 n1 ts1

instance Show Predicate where
  show = T.unpack . showTxt

instance ShowTxt Predicate where
  showTxt = fmtPredicate False

-- | Format predicates.
fmtPredicate
  :: Bool -- ^ If true, will always show parens even for predicates without arguments.
  -> Predicate -- ^ The predicate to format.
  -> T.Text -- ^ Resulting text.
fmtPredicate parens (Predicate n ts) =
  T.concat [n, if null ts then noterms else T.concat ["(", terms, ")"]]
  where terms = FT.mkString $ map showTxt ts
        noterms = if parens then "()" else ""

-- | Gathers the constants in a predicate.
constants :: Predicate -> Set T.Text
constants (Predicate _ ts) =
  foldl' (\a t -> Set.union (Term.constants t) a) Set.empty ts

-- | Shows the internal structure of the predicate.
showStruct :: Predicate -> T.Text
showStruct (Predicate n ts) =
  T.concat ["Predicate ", n, " [", if null ts then "" else terms, "]"]
  where terms = FT.mkString (map Term.showStruct ts)

-- | Tests if the term is 'grounded', i.e. if it has no variables.
ground :: Predicate -> Bool
ground (Predicate _ ts) = all Term.ground ts

-- | Tests if the predicate has a certain variable.
hasVar :: T.Text -> Predicate -> Bool
hasVar v (Predicate _ ts) = any (Term.hasVar v) ts

-- | Replace a term with another.
substitute :: Term -> Term -> Predicate -> Predicate
substitute t0 t1 (Predicate n ts) = Predicate n $ map (Term.substitute t0 t1) ts
