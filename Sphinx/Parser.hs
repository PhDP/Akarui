module Sphinx.Parser (
  parseFOL
) where

import Data.Functor.Identity
import Data.Char (isLower)
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Sphinx.Formula
import Sphinx.FOL

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

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: ParsecT String () Identity String
identifier = Tok.identifier lexer

commaSep :: ParsecT String () Identity a -> ParsecT String () Identity [a]
commaSep = Tok.commaSep lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseFOL :: String -> Either ParseError (Formula (Predicate String))
parseFOL = parse (contents parseAll) "<stdin>"

parseAll :: Parser (Formula (Predicate String))
parseAll = Ex.buildExpressionParser tbl parseAtoms

parseAtoms :: Parser (Formula (Predicate String))
parseAtoms =
      parseTop
  <|> parseBottom
  <|> parsePredicate
  <|> parens parseAll

parseTop, parseBottom :: Parser (Formula (Predicate t))
parseTop  = reserved "True" >> return Top
parseBottom = reserved "False" >> return Bottom

parsePredicate :: Parser (Formula (Predicate String))
parsePredicate = do
  n <- identifier
  reservedOp "("
  ts <- commaSep parseTerm
  reservedOp ")"
  return (Atom (Predicate n ts))

parseTerm :: Parser (Term String)
parseTerm = do
  n <- identifier
  return (if isLower $ head n then Variable n else Constant n)

reservedOps :: [String] -> ParsecT String () Identity ()
reservedOps names = foldr1 (<|>) $ map reservedOp names

-- Prefix operators
tbl :: Ex.OperatorTable String () Identity (Formula (Predicate t))
tbl =
  let
    binary ns fun = Ex.Infix (do { reservedOps ns; return fun })
    prefix ns fun = Ex.Prefix (do { reservedOps ns; return fun })
  in
    [ [prefix ["not", "NOT", "~", "!", "¬"] Not]
    , [binary ["and", "AND", "∧"] (BinOp And) Ex.AssocRight]
    , [binary ["or", "OR", "∨", "v"] (BinOp Or) Ex.AssocRight]
    , [binary ["implies", "IMPLIES", "⇒", "=>"] (BinOp Implies) Ex.AssocRight]
    , [binary ["xor", "XOR", "⊕"] (BinOp Xor) Ex.AssocRight]
    , [binary ["iff", "IFF", "⇔", "<=>"] (BinOp Iff) Ex.AssocRight] ]
