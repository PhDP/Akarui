-- | Type and functions for first-order predicate logic.
module Faun.FOL where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (foldl')
import Data.Functor.Identity
import Faun.Formula
import Faun.Predicate (Predicate (Predicate))
import qualified Faun.Predicate as Pred
import Faun.Term (Term (Constant, Variable, Function), parseTerm, parseFunForm)
import qualified Faun.Term as Term
import Faun.Parser
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex

-- | A first-order logic formula is simply a formula of predicates.
type FOL t = Formula (Predicate t)

-- | Extracts predicates from a list of formulas. If a formula is not an atom,
-- it will be ignored.
toPredicates :: (Ord t) => [FOL t] -> [Predicate t]
toPredicates = foldl' (\acc f -> case f of Atom p -> p : acc; _ -> acc) []

-- | Tests if the formula is 'grounded', i.e. if it has no variables.
ground :: FOL t -> Bool
ground f = case f of
  Atom (Predicate _ ts) -> all Term.ground ts
  BinOp _ x y           -> ground x || ground y
  Qualifier _ _ x       -> ground x
  _                     -> False

-- | Gathers all the variables in a first-order logic formula.
variables :: (Ord t) => FOL t -> Set t
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
      Qualifier _ _ x       -> Set.union (gatE x) s
      _                     -> Set.empty
    -- Gathers with an empty set
    gatE = gat Set.empty

-- | Test for the presence of a predicate in the formula.
hasPred :: (Eq t) => Predicate t -> FOL t -> Bool
hasPred p f = case f of
  Atom p'         -> p == p'
  BinOp _ x y     -> hasPred p x || hasPred p y
  Qualifier _ _ x -> hasPred p x
  _               -> False

-- | Test for the presence of a predicate in the formula using only the name
-- of the predicate.
hasPredName :: (Eq t) => FOL t -> String -> Bool
hasPredName f n = case f of
  Atom (Predicate n' _) -> n == n'
  BinOp _ x y     -> hasPredName x n || hasPredName y n
  Qualifier _ _ x -> hasPredName x n
  _               -> False

-- | Returns true if the formula has functions. This is often used in algorithms
-- where we must ensure all functions have been resolved to an object.
hasFun :: FOL t -> Bool
hasFun f = case f of
  Atom (Predicate _ ts) -> any (\trm -> (Term.numFuns trm :: Int) > 0) ts
  BinOp _ x y           -> hasFun x || hasFun y
  Qualifier _ _ x       -> hasFun x
  _                     -> False

-- | Substitute a term in the formula.
substitute :: (Eq a) => Term a -> Term a -> FOL a -> FOL a
substitute old new f = case f of
  Atom (Predicate n ts)
    -> Atom $ Predicate n $ map (Term.substitute old new) ts
  Not x           -> Not $ substitute old new x
  BinOp b x y     -> BinOp b (substitute old new x) (substitute old new y)
  Qualifier q v x -> Qualifier q v (substitute old new x)
  _               -> f -- Top / Bottom

-- | Shows the internal structure of the first-order logic formula. This is
-- mostly useful for testing and making sure the formula has the correct
-- structure.
showFOLStruct :: (Show a) => FOL a -> String
showFOLStruct f = case f of
  Atom a          -> Pred.showStruct a
  Top             -> "Top"
  Bottom          -> "Bottom"
  Not x           -> "Not (" ++ showFOLStruct x ++ ")"
  BinOp b x y     -> show b ++ " (" ++ showFOLStruct x ++ ") (" ++ showFOLStruct y ++ ")"
  Qualifier q v x -> show q ++ " " ++ v ++ "(" ++ showFOLStruct x ++ ")"

-- | Resolve functions by providing a map from function name & arguments to terms.
resolveFun :: (Ord a) => Map (String, [Term a]) (Term a) -> FOL a -> FOL a
resolveFun m f = case f of
  Atom (Predicate n ts) ->
    Atom $ Predicate n $ map (Term.resolveFun m) ts
  Not x             -> Not (resolveFun m x)
  BinOp b x y       -> BinOp b (resolveFun m x) (resolveFun m y)
  Qualifier q v x   -> Qualifier q v (resolveFun m x)
  _                 -> f

-- | Resolve predicates by providing a map from name & arguments to bool.
resolvePre :: (Ord a) => Map (String, [Term a]) Bool -> FOL a -> FOL a
resolvePre m f = case f of
  Atom (Predicate n ts) -> case Map.lookup (n, ts) m of
    Just True   -> Top
    Just False  -> Bottom
    Nothing     -> f
  Not x             -> Not (resolvePre m x)
  BinOp b x y       -> BinOp b (resolvePre m x) (resolvePre m y)
  Qualifier q v x   -> Qualifier q v (resolvePre m x)
  _                 -> f

-- | Resolves universal qualifiers, substituting the variables in the 'ForAll'
-- for a given term (a constant, generally).
resolveForAll :: String -> Term String -> FOL String -> FOL String
resolveForAll v t f = case f of
  Not x                 -> Not $ resolveForAll v t x
  BinOp b x y           -> BinOp b (resolveForAll v t x) (resolveForAll v t y)
  Qualifier ForAll v' x  ->
    if v == v' then substitute (Variable v) t x
    else Qualifier ForAll v' (resolveForAll v t x)
  Qualifier Exists v' x  -> Qualifier Exists v' (resolveForAll v t x)
  _                     -> f

-- | Takes a formula, a map between functions and constants, and a list of
-- constants to produce a set of groundings.
--
-- Reference:
--   P Domingos and D Lowd, Markov Logic: An Interface Layer for Artificial
-- Intelligence, 2009, Morgan & Claypool. p. 14.
groundings :: Map (String, [Term String]) (Term String) -> [Term String] -> FOL String -> Set (FOL String)
groundings m cs f = loopV
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
      Qualifier q v' x -> Qualifier q v' (groundSub v' x)
      _                -> f'

    existsVar f' = case f' of
      Not x                 -> Not $ existsVar x
      BinOp b x y           -> BinOp b (existsVar x) (existsVar y)
      Qualifier Exists v x  -> existsVar $ groundSub v x
      Qualifier ForAll v x  -> Qualifier ForAll v $ existsVar x
      _                     -> f'

    f0 = existsVar f
    g0 = Set.fromList [f0]
    vs = uniQualVars f0

    loopV = Set.foldr' loopG g0 vs
    loopG v g = Set.foldr (\x a -> Set.union a (Set.fromList x)) Set.empty (gr v g)
      where
        gr v' =
          Set.map (\fm -> map (\c -> simplify $ resolveFun m $ resolveForAll v' c fm) cs)

-- | Parser for weighted first-order logic. Parses a double following by
-- a formula (or a formula followed by a double).
--
-- The /smoking/ example for Markov logic:
--
-- @
--    parseWFOL \"∀x∀y∀z Friend(x, y) ∧ Friend(y, z) ⇒ Friend(x, z) 0.7\"
--    parseWFOL \"∀x Smoking(x) ⇒ Cancer(x) 1.5\"
--    parseWFOL \"1.1 ∀x∀y Friend(x, y) ∧ Smoking(x) ⇒ Smoking(y)\"
-- @
parseWFOL :: String -> Either ParseError (FOL String, Double)
parseWFOL = parse (contents parseWeighted) "<stdin>"

-- | Parser for first-order logic. The parser will read a string and output
-- an either type with (hopefully) the formula on the right.
--
-- This parser makes the assumption that variables start with a lowercase
-- character, while constants start with an uppercase character.
--
-- Some examples of valid strings for the parser:
--
-- @
--    parseFOL \"ForAll x, y PositiveInteger(y) => GreaterThan(Add(x, y), x)\"
--    parseFOL \"A.x,y: Integer(x) and PositiveInteger(y) => GreaterThan(Add(x, y), x)\"
--    parseFOL \"∀ x Add(x, 0) = x\"
-- @
parseFOL :: String -> Either ParseError (FOL String)
parseFOL = parse (contents parseFOLAll) "<stdin>"

parseFOLAll, parseSentence, parseTop, parseBottom, parseAtoms, parsePred, parsePredLike, parseIdentity, parseNIdentity, parseQual, parseNQual, parseNegation :: Parser (FOL String)
parseFOLAll = try parseNQual <|> try parseQual <|> parseSentence

parseSentence = Ex.buildExpressionParser tbl parseAtoms

parseTop  = reservedOps ["True", "TRUE", "true", "T", "⊤"] >> return Top

parseBottom = reservedOps ["False", "FALSE", "false", "F", "⊥"] >> return Bottom

parseNQual = do
  nots <- many1 parseNot
  (q, vs, a) <- parseQualForm
  return $ foldr (\_ acc -> Not acc) (foldr (Qualifier q) a vs) nots

parseQual = do
  (q, vs, a) <- parseQualForm
  return $ foldr (Qualifier q) a vs

parseNegation = do
  n <- parseNot
  a <- parseAtoms
  return $ n a

parsePredLike = try parseIdentity <|> try parseNIdentity <|> parsePred

parseAtoms =
      try parsePredLike
  <|> parseNegation
  <|> parseTop
  <|> parseBottom
  <|> parens parseFOLAll

parsePred = do
  args <- parseFunForm
  return $ Atom $ uncurry Predicate args

parseIdentity = do
  left <- parseTerm
  reservedOps ["=", "=="]
  right <- parseTerm
  return $ Atom $ Predicate "Identity" [left, right]

parseNIdentity = do
  left <- parseTerm
  reservedOps ["!=", "/=", "\\neq"]
  right <- parseTerm
  return $ Not $ Atom $ Predicate "Identity" [left, right]

parseNot :: Parser (FOL String -> FOL String)
parseNot = reservedOps ["Not", "NOT", "not", "~", "!", "¬"] >> return Not

parseExists, parseForAll :: Parser QualT
parseExists = reservedOps ["E.", "Exists", "exists", "∃"] >> return Exists
parseForAll = reservedOps ["A.", "ForAll", "Forall", "forall", "∀"] >> return ForAll

-- Parse a weight and then a first-order logic formula
parseLeftW :: Parser (FOL String, Double)
parseLeftW = do
  n <- float
  f <- parseFOLAll
  return (f, n)

-- Parse a first-order logic formula and then a weight
parseRightW :: Parser (FOL String, Double)
parseRightW = do
  f <- parseFOLAll
  n <- float
  return (f, n)

parseWeighted :: Parser (FOL String, Double)
parseWeighted = try parseLeftW <|> parseRightW

parseQualForm :: Parser (QualT, [String], FOL String)
parseQualForm = do
  q <- parseExists <|> parseForAll -- many1
  v <- commaSep identifier
  optional $ reservedOp ":"
  a <- parseFOLAll
  return (q, v, a)

-- Prefix operators
tbl :: Ex.OperatorTable String () Identity (Formula a)
tbl =
  [ [binary ["And", "and", "AND", "∧"] (BinOp And) Ex.AssocRight]
  , [binary ["Or", "or", "OR", "∨", "v"] (BinOp Or) Ex.AssocRight]
  , [binary ["Implies", "implies", "IMPLIES", "⇒", "=>"] (BinOp Implies) Ex.AssocRight]
  , [binary ["Xor", "xor", "XOR", "⊕"] (BinOp Xor) Ex.AssocRight]
  , [binary ["Iff", "iff", "IFF", "⇔", "<=>"] (BinOp Iff) Ex.AssocRight] ]
  where binary ns fun = Ex.Infix (do { reservedOps ns; return fun })
