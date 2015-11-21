-- | Parsers for first-order logic and other important structures (e.g. Markov
-- logic networks).
module Sphinx.Parser (
  parseFOL,
  parseWFOL,
  parseJointQuery,
  parseCondQuery,
  parsePredicate,
  parsePredicateAss,
  parseEvidenceList,
  parseEvidenceLines,
  parseClause,
  parseWClause,
  parseDomain
) where

import Data.Functor.Identity
import Data.Char (isLower)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Data.List (foldl')
import qualified Data.Set as Set
import Data.Set (Set)
import Sphinx.Formula
import Sphinx.FOL
import Sphinx.Term
import Sphinx.Predicate
import Sphinx.Clause

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
parseCondQuery :: String -> Either ParseError (Set (Predicate String, Bool), Set (Predicate String, Bool))
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
parseJointQuery :: String -> Either ParseError (Set (Predicate String, Bool))
parseJointQuery = parse (contents parseJ) "<stdin>"

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
parsePredicateAss :: String -> Either ParseError (Predicate String, Bool)
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
parseEvidenceList :: String -> Either ParseError [(Predicate String, Bool)]
parseEvidenceList = parse (contents parseEviList) "<stdin>"

-- | Parse a clause (a disjunction of positive and negative literals).
--
-- @
--    !Women(p) or Vegetarian(p)
-- @
parseClause :: String -> Either ParseError (Clause (Predicate String))
parseClause = parse (contents parseCl) "<stdin>"

-- | Parse a weighted clause (a disjunction of positive and negative literals).
--
-- @
--    1.5 !Women(p) or Vegetarian(p)
--    !Women(p) or Vegetarian(p)  1.5
-- @
parseWClause :: String -> Either ParseError (Clause (Predicate String), Double)
parseWClause = parse (contents parseWeightedClause) "<stdin>"

-- | Parse a clause (a disjunction of positive and negative literals).
--
-- @
--    dom1={1, 2, 3, 4}
--    person = { Elaine, George, Jerry, Cosmo, Newman }
-- @
parseDomain :: String -> Either ParseError (String, Set String)
parseDomain = parse (contents parseDs) "<stdin>"

-- | Parse a list of evidence separated by spaces (or newline characters).
-- uses the same syntax as 'parseEvidenceList', except only spaces and newline
-- characters can separate the predicates.
parseEvidenceLines :: String -> Either ParseError [(Predicate String, Bool)]
parseEvidenceLines = parse (contents parseEviLines) "<stdin>"

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef {
    Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = False
  , Tok.identStart      = alphaNum
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":#$%&*+./<=>?@\\^|-"
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

symbol :: String -> Parser String
symbol = Tok.symbol lexer

float, pfloat, nfloat :: ParsecT String () Identity Double

float = nfloat <|> pfloat

nfloat = do
  reservedOp "-"
  f <- Tok.float lexer
  return (-f)


pfloat = do
  optional $ reservedOp "+"
  f <- Tok.float lexer
  return f

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

parseCl :: Parser (Clause (Predicate String))
parseCl = do
  optional $ reservedOp "("
  ls <- parsePredTruth `sepBy` (symbol "v" <|> symbol "or" <|> symbol "∨" <|> symbol "|")
  optional $ reservedOp ")"
  let ps = foldl' (\a (p, b) -> if b then Set.insert p a else a) Set.empty ls
      ns = foldl' (\a (p, b) -> if not b then Set.insert p a else a) Set.empty ls in
    return $ Clause ps ns

-- Parse a weight and then a first-order logic formula
parseLeftWC :: Parser (Clause (Predicate String), Double)
parseLeftWC  = do
  n <- float
  c <- parseCl
  return (c, n)

-- Parse a first-order logic formula and then a weight
parseRightWC :: Parser (Clause (Predicate String), Double)
parseRightWC = do
  c <- parseCl
  n <- float
  return (c, n)

parseWeightedClause :: Parser (Clause (Predicate String), Double)
parseWeightedClause = try parseLeftWC <|> parseRightWC

parseDs :: Parser (String, Set String)
parseDs = do
  n <- identifier
  reservedOp "="
  reservedOp "{"
  elems <- commaSep identifier
  reservedOp "}"
  return (n, Set.fromList elems)

parseEviList :: Parser [(Predicate String, Bool)]
parseEviList = parsePredTruth `sepBy` (symbol "," <|> symbol ";" <|> symbol "and" <|> symbol "∩")

parseEviLines :: Parser [(Predicate String, Bool)]
parseEviLines = many1 parsePredTruth

parseQualForm :: Parser (QualT, [String], FOL String)
parseQualForm = do
  q <- parseExists <|> parseForAll -- many1
  v <- commaSep identifier
  optional $ reservedOp ":"
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
      try parseNegPred
  <|> try parsePredAss
  <|> do { p <- parsePredOnly; return (p, True) }

parseNegPred :: Parser (Predicate String, Bool)
parseNegPred = do
  reservedOps ["!", "~"]
  p <- parsePredOnly
  return (p, False)

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

parseJ :: Parser (Set (Predicate String, Bool))
parseJ = do
  reservedOps ["P(", "p(", "Probability(", "probability("]
  query <- parseEviList
  reservedOp ")"
  return $ Set.fromList query

-- Parse conditionals P(f1 | f2 -> true, f3 -> False, f4 -> T).
parseQ :: Parser (Set (Predicate String, Bool), Set (Predicate String, Bool))
parseQ = do
  reservedOps ["P(", "p(", "Probability(", "probability("]
  query <- parseEviList
  reservedOps ["|", "\\mid", "given"]
  conds <- parseEviList
  reservedOp ")"
  return (Set.fromList query, Set.fromList conds)

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
