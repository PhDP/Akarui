module Sphinx.Symbols where

-- List of symbols
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

long, shouting, semisymbolic, symbolic, laTeX, ascii :: Symbols
long = Symbols "and" "or" "xor" "implies" "iff" "not" "true" "false" "for all"
  "exists" "!="
shouting = Symbols "AND" "OR" "XOR" "IMPLIES" "IFF" "NOT" "TRUE" "FALSE"
  "FORALL" "EXISTS" "!="
semisymbolic = Symbols "∧" "∨" "⊕" "⇒" "⇔" "¬" "T" "F" "∀" "∃" "!="
symbolic = Symbols "∧" "∨" "⊕" "⇒" "⇔" "¬" "⊤" "⊥" "∀" "∃" "!="
laTeX = Symbols "\\land" "\\lor" "\\oplus" "\\Rightarrow" "\\iff" "\\lneg" "T"
  "F" "\\forall" "\\exists" "\\neq"
ascii = Symbols "/\\" "\\/" "(+)" "=>" "<=>" "~" "true" "false" "for all"
  "exists" "!="
