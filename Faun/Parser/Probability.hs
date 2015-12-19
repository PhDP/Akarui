-- | Parser to handle joint and conditional queries.
module Faun.Parser.Probability (
  parseEvidenceLines,
  parseEvidenceList,
  parsePredicateAss,
  parsePredicate,
  parseJointQuery,
  parseCondQuery
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Set as Set
import Data.Set (Set)
import Faun.Parser.Core
import Faun.Parser.FOL
import Faun.Parser.Bool
import Faun.Formula
import Faun.Predicate

-- | Parser for conditional queries of the form
-- P(f0 = v0, f1 = v1 | f2 = v2, f3 = v3, ...), where f, f0, f1, f2 are
-- first-order logic predicates and v0, v1, v3 are optional boolean values
-- (True, False, T, F). The parser is fairly flexible (see examples), allowing
-- you to omit the assignment (in which case it is assumed to be true) and
-- use various symbols for joint probabilities.
--
-- For truth values, this parser accepts T TRUE True true ⊤ F False FALSE false ⊥.
--
-- For introducing the truth value after a variable (e.g. Smoking(Bob) = True), the parser
-- accepts == = := is ->. It is entirely optional as a variable without assignment
-- is assumed to be true so
--
-- @
--    Smoking(Bob) -> T
--    Smoking(Bob)
-- @
--
-- are equivalent. Also, it's possible to introduce negation with the ~ or ! suffix, so
--
-- @
--    Smoking(Bob) = ⊥
--    !Smoking(Bob)
-- @
--
-- are also equivalent.
--
-- For separating variables in joint probabilities, the parser accetps , ; and ∩.
-- For introducing conditioned variables, either use the traditional |, LaTeX' \mid,
-- or the word /given/.
--
-- Full examples:
--
-- @
--    parseCondQuery \"P(Predators(Wolf, Rabbit) | SameLocation(Wolf, Rabbit), Juicy(Rabbit))\"
--    parseCondQuery \"P(!Predators(Rabbit, Wolf) | EatLettuce(Rabbit) ∩ EatLettuce(Wolf) = False)\"
--    parseCondQuery \"Probability(Smoking(Bob) given Smoking(Anna) -> true, Friend(Anna, Bob) is false)\"
-- @
parseCondQuery :: String -> Either ParseError (Set (Predicate, Bool), Set (Predicate, Bool))
parseCondQuery = parse (contents parseQ) "<stdin>"

-- | Parser for joint probabilitye queries of the form
-- P(f0 = v0, f1 = v1, ...), where f0, f1 et al. are first-order logic
-- predicates formulas, and v0, v1, ...are optional boolean values (True,
-- False, T, F). This parser uses the same syntax as 'parseCondQuery', without
-- the conditional variables.
--
-- @
--    parseJointQuery \"Probability(FluInfection(Dan) ∩ StarLord(Dan) ∩ ElvisLivesIn(Sherbrooke))\"
--    parseJointQuery \"P(Cancer(Charlotte), Cancer(Anna))\"
-- @
parseJointQuery :: String -> Either ParseError (Set (Predicate, Bool))
parseJointQuery = parse (contents parseJ) "<stdin>"

-- | Parser for predicates.
--
-- @
--    Predators(Wolf, Rabbit)
--    GreaterThan(Add(1, x), 0)
-- @
parsePredicate :: String -> Either ParseError Predicate
parsePredicate = parse (contents parsePredOnly) "<stdin>"

-- | Parser for predicates assigned to a truth value. If a truth value is not
-- included, the parser assumes it is true.
--
-- For truth values, this parser accepts T TRUE True true ⊤ F False FALSE false ⊥.
--
-- For introducing the truth value after a variable (e.g. Smoking(Bob) = True), the parser
-- accepts == = := is ->. It's also possible to prefix the predicate with either
-- ! or ~ for negations.
--
-- @
--    Predators(Rabbit, Wolf) = False
--    !Predators(Rabbit, Wolf)
--    GreaterThan(Add(1, x), 0)
--    Equals(2, 2) is true
--    Foo(bar, baz) == F
-- @
parsePredicateAss :: String -> Either ParseError (Predicate, Bool)
parsePredicateAss = parse (contents parsePredTruth) "<stdin>"

-- | Parse a list of evidence (predicate with optional truth value, see
-- 'parsePredicateAss') separated by one of , ; and ∩.
--
-- @
--    Predators(Wolf, Rabbit) = False, GreaterThan(Add(1, x), 0)
--    A() ∩ B() ∩ C() ∩ !D()
--    Equals(2, 2) is true
--    Foo(bar, baz) == F, Grrr()
-- @
parseEvidenceList :: String -> Either ParseError [(Predicate, Bool)]
parseEvidenceList = parse (contents parseEviList) "<stdin>"

-- | Parse a list of evidence separated by spaces (or newline characters).
-- uses the same syntax as 'parseEvidenceList', except only spaces and newline
-- characters can separate the predicates.
parseEvidenceLines :: String -> Either ParseError [(Predicate, Bool)]
parseEvidenceLines = parse (contents parseEviLines) "<stdin>"

parseEviList :: Parser [(Predicate, Bool)]
parseEviList = parsePredTruth `sepBy` (symbol "," <|> symbol ";" <|> symbol "and" <|> symbol "∩")

parseEviLines :: Parser [(Predicate, Bool)]
parseEviLines = many1 parsePredTruth

parsePredTruth :: Parser (Predicate, Bool)
parsePredTruth =
      try parseNegPred
  <|> try parsePredAss
  <|> do { p <- parsePredOnly; return (p, True) }

parseNegPred :: Parser (Predicate, Bool)
parseNegPred = do
  reservedOps ["!", "~"]
  p <- parsePredOnly
  return (p, False)

parsePredOnly :: Parser Predicate
parsePredOnly = do
  f <- parsePred
  return $ case f of Atom p -> p; _ -> Predicate "" []

parsePredAss :: Parser (Predicate, Bool)
parsePredAss = do
  p <- parsePredOnly
  reservedOps ["->", "=", "==", ":=", "is"]
  t <- getTop <|> getBot
  return (p, t)

parseJ :: Parser (Set (Predicate, Bool))
parseJ = do
  reservedOps ["P(", "p(", "Probability(", "probability("]
  query <- parseEviList
  reservedOp ")"
  return $ Set.fromList query

-- Parse conditionals P(f1 | f2 -> true, f3 -> False, f4 -> T).
parseQ :: Parser (Set (Predicate, Bool), Set (Predicate, Bool))
parseQ = do
  reservedOps ["P(", "p(", "Probability(", "probability("]
  query <- parseEviList
  reservedOps ["|", "\\mid", "given"]
  conds <- parseEviList
  reservedOp ")"
  return (Set.fromList query, Set.fromList conds)
