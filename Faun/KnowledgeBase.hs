-- | A knowledge base is a set of formulas. Will replace KB.
module Faun.KnowledgeBase where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Faun.Formula (allAss, satisfy)
import Faun.FOL
--import Faun.Predicate
import Faun.RuleType
import Faun.Domain
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

-- | Checks whether a formulas entails.
entails :: KnowledgeBase -> FOL String -> Bool
entails k f = all (\a -> allKeys (satisfy a) (formulas k)) ass
  where
    ass = allAss $ Set.insert f $ Map.keysSet $ formulas k

