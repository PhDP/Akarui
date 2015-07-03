{-# LANGUAGE OverloadedStrings #-}

-- | Parsers for first-order logic and other important structures (e.g. Markov
-- logic networks).
module Manticore.Parser (
  parseFOL,
  parseWFOL,
  parseCondQuery,
  parsePredicate,
  parsePredicateAss
) where

import Data.Functor.Identity
import Data.Char (isLower)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Data.Map as Map
import Data.Map (Map)
import Manticore.Formula
import Manticore.FOL
import Manticore.Term
import Manticore.Predicate

-- | Parser for weighted first-order logic. Parses a double following by
-- a formula (or a formula followed by a double).
--
-- The /smoking/ example for Markov logic:
--
-- @
--    ∀x∀y∀z Fr(x, y) ∧ Fr(y, z) ⇒ Fr(x, z) 0.7
--    ∀x Sm(x) ⇒ Ca(x) 1.5
--    1.1 ∀x∀y Fr(x, y) ∧ Sm(x) ⇒ Sm(y)
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
--    parseFOL "ForAll .x, y PositiveInteger(y) => GreaterThan(Add(x, y), x)"
--    parseFOL "A.x,y PositiveInteger(y) => GreaterThan(Add(x, y), x)"
--    parseFOL "∀ x Add(x, 0) = x"
-- @
parseFOL :: String -> Either ParseError (FOL String)
parseFOL = parse (contents parseFOLAll) "<stdin>"

-- | Parser for conditional queries of the form
-- P(f | f0 -> v0, f1 -> v1, f2 -> v2, ...), where f, f0, f1, f2 are formulas
-- in first-order logic, and v0, v1, v3 are optional boolean values (True,
-- False, T, F).
--
-- The parser is fairly flexible (see examples), but it won't allow the equal
-- sign to attribute truth values to formulas since it conflicts with the first-
-- order logic parser.
--
-- @
--    P(Predators(Wolf, Rabbit) | SameLocation(Wolf, Rabbit), Juicy(Rabbit))
--    Probability(Smoking(Bob) given Smoking(Anna) -> true, Friend(Anna, Bob) is false)
-- @
parseCondQuery :: String -> Either ParseError (Map (FOL String) Bool, Map (FOL String) Bool)
parseCondQuery = parse (contents parseQ) "<stdin>"

-- | Parser for predicates.
--
-- @
--    Predators(Wolf, Rabbit)
--    GreaterThan(Add(1, x), 0)
-- @
parsePredicate :: String -> Either ParseError (Predicate String)
parsePredicate = parse (contents parsePredOnly) "<stdin>"

-- | Parser for predicates assigned to a truth value. If a truth value is not
-- included, the parser assumes it is true.
--
-- @
--    Predators(Wolf, Rabbit) = False
--    GreaterThan(Add(1, x), 0)
--    Equals(2, 2) is true
--    Foo(bar, baz) == F
-- @
parsePredicateAss :: String -> Either ParseError (Predicate String, Bool)
parsePredicateAss = parse (contents parsePredTruth) "<stdin>"

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef {
    Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = False
  , Tok.identStart      = alphaNum
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reservedOp :: String -> Parser ()
--reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

float :: ParsecT String () Identity Double
float = Tok.float lexer

identifier :: ParsecT String () Identity String
identifier = Tok.identifier lexer

commaSep :: ParsecT String () Identity a -> ParsecT String () Identity [a]
commaSep = Tok.commaSep lexer

reservedOps :: [String] -> ParsecT String () Identity ()
reservedOps names = foldr1 (\x acc -> try x <|> acc) $ map reservedOp names

-- Prefix operators
tbl :: Ex.OperatorTable String () Identity (Formula a)
tbl =
  [ [binary ["And", "and", "AND", "∧"] (BinOp And) Ex.AssocRight]
  , [binary ["Or", "or", "OR", "∨", "v"] (BinOp Or) Ex.AssocRight]
  , [binary ["Implies", "implies", "IMPLIES", "⇒", "=>"] (BinOp Implies) Ex.AssocRight]
  , [binary ["Xor", "xor", "XOR", "⊕"] (BinOp Xor) Ex.AssocRight]
  , [binary ["Iff", "iff", "IFF", "⇔", "<=>"] (BinOp Iff) Ex.AssocRight] ]
  where binary ns fun = Ex.Infix (do { reservedOps ns; return fun })

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseQualForm :: Parser (QualT, [String], FOL String)
parseQualForm = do
  q <- parseExists <|> parseForAll -- many1
  v <- commaSep identifier
  a <- parseFOLAll
  return (q, v, a)

parseFunForm :: Parser (String, [Term String])
parseFunForm = do
  n <- identifier
  reservedOp "("
  ts <- commaSep parseTerm
  reservedOp ")"
  return (n, ts)

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

parsePredTruth :: Parser (Predicate String, Bool)
parsePredTruth =
      try parsePredAss
  <|> do { p <- parsePredOnly; return (p, True) }

parsePredOnly :: Parser (Predicate String)
parsePredOnly = do
  f <- parsePred
  return $ case f of Atom p -> p; _ -> Predicate "" []

parsePredAss :: Parser (Predicate String, Bool)
parsePredAss = do
  p <- parsePredOnly
  reservedOps ["->", "=", "==", ":=", "is"]
  t <- parseTop <|> parseBottom
  return (p, t == Top)

-- Parse conditionals P(f1 | f2 -> true, f3 -> False, f4 -> T).
parseQ :: Parser (Map (FOL String) Bool, Map (FOL String) Bool)
parseQ = do
  reservedOps ["P(", "p(", "Probability(", "probability("]
  query <- commaSep (try parseAssFOL <|> do { f <- parseFOLAll; return (f, True) })
  reservedOps ["|", "\\mid", "given"]
  conds <- commaSep (try parseAssFOL <|> do { f <- parseFOLAll; return (f, True) })
  reservedOp ")"
  return (Map.fromList query, Map.fromList conds)

parseAssFOL :: Parser (FOL String, Bool)
parseAssFOL = do
  f <- parseFOLAll
  reservedOps ["->", ":", "is"]
  v <- parseTop <|> parseBottom
  return (f, v == Top)

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

parseTerm, parseVarCon, parseFunction :: Parser (Term String)
parseTerm = try parseFunction <|> parseVarCon

parseFunction = do
  args <- parseFunForm
  return $ uncurry Function args

parseVarCon = do
  n <- identifier
  return $ if isLower $ head n then Variable n else Constant n
