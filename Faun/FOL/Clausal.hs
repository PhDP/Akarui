-- | Defines a type class for clausal forms.
module Faun.FOL.Clausal where

-- class (KnowledgeBase c) => Clause c where
class Clausal c where
  -- | Number of positive literals in the clause.
  numPositive :: c -> Int

  -- | Number of negative literals in the clause.
  numNegative :: c -> Int

  -- | Number of literals in the clause
  numLiterals :: c -> Int
  numLiterals x = numPositive x + numNegative x

  -- | Returns true for empty clauses.
  isEmpty :: c -> Bool
  isEmpty x = numLiterals x == 0

  -- | A definite clause has only one positive literal.
  isDefinite :: c -> Bool
  isDefinite x = numPositive x == 1

  -- | A fact has only one positive literal and no negative literals.
  isFact :: c -> Bool
  isFact x = numPositive x == 1 && numNegative x == 0

  -- | A rule has one positive literal and one or more negative literals.
  isRule :: c -> Bool
  isRule x = numPositive x == 1 && numNegative x > 0

  -- | A query has no positive literals and one or more negative literals.
  isQuery :: c -> Bool
  isQuery x = numPositive x == 0 && numNegative x > 0

  -- | Horn clauses have 0 or 1 positive literals.
  isHorn :: c -> Bool
  isHorn x = numPositive x < 2
