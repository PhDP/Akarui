-- | Type and functions for first-order predicate logic.
module Faun.Parser.FOL where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import Faun.FOL.Formula
import Faun.FOL.FOL
import Faun.Parser.LogicOps
import Faun.Parser.Core
import Faun.Parser.Numbers
import Faun.Parser.Term as Term
import Faun.FOL.Predicate
import Faun.FOL.QuanT

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
parseWFOL :: String -> Either ParseError (FOL, Double)
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
parseFOL :: String -> Either ParseError FOL
parseFOL = parse (contents parseFOLAll) "<stdin>"

parseFOLAll, parseSentence, parseTop, parseBot, parseAtoms, parsePred, parsePredLike, parseIdentity, parseNIdentity, parseQuan, parseNQuan, parseNegation :: Parser FOL
parseFOLAll = try parseNQuan<|> try parseQuan <|> parseSentence

parseSentence = Ex.buildExpressionParser logicTbl parseAtoms

parseTop  = reservedOps ["True", "TRUE", "true", "T", "⊤"] >> return top

parseBot = reservedOps ["False", "FALSE", "false", "F", "⊥"] >> return bot

parseNQuan = do
  nots <- many1 parseNot
  (q, vs, a) <- parseQuanForm
  return $ foldr (\_ acc -> Not acc) (foldr (Quantifier q) a vs) nots

parseQuan = do
  (q, vs, a) <- parseQuanForm
  return $ foldr (Quantifier q) a vs

parseNegation = do
  n <- parseNot
  a <- parseAtoms
  return $ n a

parsePredLike = try parseIdentity <|> try parseNIdentity <|> parsePred

parseAtoms =
      try parsePredLike
  <|> parseNegation
  <|> parseTop
  <|> parseBot
  <|> parens parseFOLAll

parsePred = do
  args <- Term.parseFunForm
  return $ Atom $ uncurry Predicate args

parseIdentity = do
  left <- Term.parseTerm
  reservedOps ["=", "=="]
  right <- Term.parseTerm
  return $ Atom $ Predicate "Identity" [left, right]

parseNIdentity = do
  left <- Term.parseTerm
  reservedOps ["!=", "/=", "\\neq"]
  right <- Term.parseTerm
  return $ Not $ Atom $ Predicate "Identity" [left, right]

parseNot :: Parser (FOL -> FOL)
parseNot = reservedOps ["Not", "NOT", "not", "~", "!", "¬"] >> return Not

parseExists, parseForAll :: Parser QuanT
parseExists = reservedOps ["E.", "Exists", "exists", "∃"] >> return Exists
parseForAll = reservedOps ["A.", "ForAll", "Forall", "forall", "∀"] >> return ForAll

-- Parse a weight and then a first-order logic formula
parseLeftW :: Parser (FOL, Double)
parseLeftW = do
  n <- getDouble
  f <- parseFOLAll
  return (f, n)

-- Parse a first-order logic formula and then a weight
parseRightW :: Parser (FOL, Double)
parseRightW = do
  f <- parseFOLAll
  n <- getDouble
  return (f, n)

parseWeighted :: Parser (FOL, Double)
parseWeighted = try parseLeftW <|> parseRightW

parseQuanForm :: Parser (QuanT, [T.Text], FOL)
parseQuanForm = do
  q <- parseExists <|> parseForAll -- many1
  v <- commaSep identifier
  optional $ reservedOp ":"
  a <- parseFOLAll
  return (q, map T.pack v, a)
