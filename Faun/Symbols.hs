-- | Different sets of symbols used to print logic formulas.
module Faun.Symbols where

-- | Supported symbols
data Symbols = Symbols
  { symAnd :: String
  , symOr :: String
  , symXor :: String
  , symImplies :: String
  , symIff :: String
  , symNot :: String
  , symTop :: String
  , symBottom :: String
  , symForall :: String
  , symExists :: String
  , symNotEqual :: String
  } deriving (Show)

human, long, shouting, semisymbolic, symbolic, laTeX, ascii :: Symbols

-- | A representation using words instead of symbols (blasphemy!).
human = Symbols "and" "or" "xor" "=>" "iff" "!" "true" "false" "Forall"
  "Exists" "!="

-- | A representation using words instead of symbols (blasphemy!).
long = Symbols "and" "or" "xor" "implies" "iff" "not" "true" "false" "Forall"
  "Exists" "!="

-- | USING WORDS INSTEAD OF SYMBOLS AND BEING LOUD ABOUT IT!
shouting = Symbols "AND" "OR" "XOR" "IMPLIES" "IFF" "NOT" "TRUE" "FALSE"
  "FORALL" "EXISTS" "!="

-- | Mostly symbolc representation, except for true and false (top and bottom).
semisymbolic = Symbols "∧" "∨" "⊕" "⇒" "⇔" "¬" "T" "F" "∀" "∃" "!="

-- | Purely symbolic representation.
symbolic = Symbols "∧" "∨" "⊕" "⇒" "⇔" "¬" "⊤" "⊥" "∀" "∃" "!="

-- | LaTeX codes for logic symbols.
laTeX = Symbols "\\land" "\\lor" "\\oplus" "\\Rightarrow" "\\iff" "\\lnot" "T"
  "F" "\\forall" "\\exists" "\\neq"

-- | An ASCII representation inspired by Harris' automated reasoning book.
ascii = Symbols "/\\" "\\/" "(+)" "=>" "<=>" "~" "true" "false" "for all"
  "exists" "!="
