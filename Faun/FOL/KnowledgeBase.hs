-- | A knowledge base is a set of formulas. Will replace KB.
module Faun.FOL.KnowledgeBase where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Faun.FOL.Formula (allAss, satisfy)
import Faun.FOL.FOL
--import Faun.Predicate
import Faun.FOL.RuleType
import Faun.FOL.Domain
import Faun.Utils (allKeys)

-- | A knowledge base is a set of formulas.
data KnowledgeBase = KnowledgeBase
  -- | All formulas with their types.
  { formulas          :: Map (FOL String) RuleType
  -- | Set of domains used in the knowledge base.
  , domains           :: Set (Domain)
  -- | Maps the predicate (by name) to their domain.
  , predicates        :: Map String [Domain]
  }

-- | Checks by brute force if a knowledgebase entails a formula.
(|=) :: KnowledgeBase -> FOL String -> Bool
(|=) k f = all (\a -> not (allKeys (satisfy a) (formulas k)) || satisfy a f) ass
  where
    ass = allAss $ Set.insert f $ Map.keysSet $ formulas k
