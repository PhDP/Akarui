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

reserved, reservedOp :: String -> Parser ()
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

identifier :: ParsecT String () Identity String
identifier = Tok.identifier lexer

commaSep :: ParsecT String () Identity a -> ParsecT String () Identity [a]
commaSep = Tok.commaSep lexer

reservedOps :: [String] -> ParsecT String () Identity ()
reservedOps names = foldr1 (<|>) $ map reservedOp names

-- Prefix operators
tbl :: Ex.OperatorTable String () Identity (FOL t)
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

parseFOL :: String -> Either ParseError (FOL String)
parseFOL = parse (contents parseAll) "<stdin>"

parseAll, parseSentence, parseTop, parseBottom, parseAtoms, parsePredicate, parseQual, parseNots :: Parser (FOL String)
parseAll = parseQual <|> parseSentence

parseSentence = Ex.buildExpressionParser tbl (parseNots <|> parseAtoms)

parseTop  = reserved "True" >> return Top

parseBottom = reserved "False" >> return Bottom

parseQual = do
  q <- parseExists <|> parseForAll -- many1
  v <- identifier
  a <- parseAll
  return $ Qualifier q v a

parseNots = do
  nots <- many1 parseNot
  a <- parseAtoms
  return $ if even $ length nots then Not a else a

parseAtoms = parseTop <|> parseBottom <|> parsePredicate <|> parens parseAll

parsePredicate = do
  args <- parseFunForm
  return $ Atom $ uncurry Predicate args

parseNot :: Parser (FOL String -> FOL String)
parseNot = reservedOps ["Not", "NOT", "not", "~", "!", "¬"] >> return Not

parseExists, parseForAll :: Parser QualT
parseExists = reservedOps ["Exists", "exists", "∃"] >> return Exists
parseForAll = reservedOps ["ForAll", "forall", "∀"] >> return ForAll

parseFunForm :: Parser (String, [Term String])
parseFunForm = do
  n <- identifier
  reservedOp "("
  ts <- commaSep parseTerm
  reservedOp ")"
  return (n, ts)

parseTerm, parseVarCon, parseFunction :: Parser (Term String)
parseTerm = try parseFunction <|> parseVarCon

parseFunction = do
  args <- parseFunForm
  return $ uncurry Function args

parseVarCon = do
  n <- identifier
  return (if isLower $ head n then Variable n else Constant n)
