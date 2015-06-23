{-# LANGUAGE OverloadedStrings #-}

-- | Parsers for first-order logic and other important structures (e.g. Markov
-- logic networks).
module Sphinx.Parser (
  parseFOL,
  parseWFOL
) where

import Data.Functor.Identity
import Data.Char (isLower)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Sphinx.Formula
import Sphinx.FOL
import Sphinx.Term
import Sphinx.Predicate

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef {
    Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = False
  , Tok.identStart      = letter
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

parseQualForm :: Parser (QualT, String, FOL String)
parseQualForm = do
  q <- parseExists <|> parseForAll -- many1
  v <- identifier
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

-- | Parser for weighted first-order logic. Parses a double following by
-- a formula (or a formula followed by a double).
parseWFOL :: String -> Either ParseError (FOL String, Double)
parseWFOL = parse (contents parseWeighted) "<stdin>"

-- | Parser for first-order logic. The parser will read a string and output
-- an either type with (hopefully) the formula on the right.
--
-- This parser makes the assumption that variables start with a lowercase
-- character, while constants start with an uppercase character.
parseFOL :: String -> Either ParseError (FOL String)
parseFOL = parse (contents parseFOLAll) "<stdin>"

parseFOLAll, parseSentence, parseTop, parseBottom, parseAtoms, parsePredicate, parseQual, parseNQual, parseNots :: Parser (FOL String)
parseFOLAll = try parseNQual <|> try parseQual <|> parseSentence

parseSentence = Ex.buildExpressionParser tbl (parseAtoms <|> parseNots)

parseTop  = reservedOps ["True", "TRUE", "true", "T", "⊤"] >> return Top

parseBottom = reservedOps ["False", "FALSE", "false", "F", "⊥"] >> return Bottom

parseNQual = do
  nots <- many1 parseNot
  (q, v, a) <- parseQualForm
  return $ foldr (\_ acc -> Not acc) (Qualifier q v a) nots

parseQual = do
  (q, v, a) <- parseQualForm
  return $ Qualifier q v a

parseNots = do
  nots <- many1 parseNot
  a <- parseAtoms
  return $ foldr (\_ acc -> Not acc) a nots

parseAtoms = try parsePredicate <|> parseTop <|> parseBottom <|> parens parseFOLAll

parsePredicate = do
  args <- parseFunForm
  return $ Atom $ uncurry Predicate args

parseNot :: Parser (FOL String -> FOL String)
parseNot = reservedOps ["Not", "NOT", "not", "~", "!", "¬"] >> return Not

parseExists, parseForAll :: Parser QualT
parseExists = reservedOps ["Exists", "exists", "∃"] >> return Exists
parseForAll = reservedOps ["ForAll", "forall", "∀"] >> return ForAll

parseTerm, parseVarCon, parseFunction :: Parser (Term String)
parseTerm = try parseFunction <|> parseVarCon

parseFunction = do
  args <- parseFunForm
  return $ uncurry Function args

parseVarCon = do
  n <- identifier
  return (if isLower $ head n then Variable n else Constant n)
