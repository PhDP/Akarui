-- | Different sets of symbols used to print logic formulas.
module Faun.Symbols where

import qualified Data.Text as T

-- | Sets of symbols to print logic formulas.
data Symbols = Symbols
  { symAnd        :: T.Text
  , symOr         :: T.Text
  , symXor        :: T.Text
  , symImplies    :: T.Text
  , symIff        :: T.Text
  , symNot        :: T.Text
  , symTop        :: T.Text
  , symBottom     :: T.Text
  , symForall     :: T.Text
  , symExists     :: T.Text
  , symNotEqual   :: T.Text
  } deriving (Show)

human, long, shouting, semisymbolic, symbolic, laTeX, ascii, setnotation :: Symbols

-- | A standard mix of symbols and strings.
human = Symbols "and" "or" "xor" "=>" "iff" "!" "true" "false" "Forall"
  "Exists" "!="

-- | A representation using standard set notation.
setnotation = Symbols "∩" "∪" "⊕" "⊃" "↔" "¬" "true" "false" "Forall"
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
