-- | Type and functions for first-order predicate logic.
module Faun.Parser.LogicOps (
  logicTbl
) where

import Data.Functor.Identity
import qualified Text.Parsec.Expr as Ex
import Faun.Parser.Core
import Faun.Formula
import Faun.BinT

-- | Operators for logic formulas. Order of precedence: and, or, implies, xor, equivalence.
logicTbl :: Ex.OperatorTable String () Identity (Formula a)
logicTbl =
  [ [binary ["And", "and", "AND", "∧"] (BinOp And) Ex.AssocRight]
  , [binary ["Or", "or", "OR", "∨", "v"] (BinOp Or) Ex.AssocRight]
  , [binary ["Implies", "implies", "IMPLIES", "⇒", "=>"] (BinOp Implies) Ex.AssocRight]
  , [binary ["Xor", "xor", "XOR", "⊕"] (BinOp Xor) Ex.AssocRight]
  , [binary ["Iff", "iff", "IFF", "⇔", "<=>"] (BinOp Iff) Ex.AssocRight] ]
  where binary ns fun = Ex.Infix (do { reservedOps ns; return fun })
