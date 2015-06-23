{-# LANGUAGE OverloadedStrings #-}

-- | A knowledge base is a set of formulas. See MLN.fs for probabilistic
-- knowledge bases.
module Manticore.KB where

--import qualified Data.Set as Set
import Data.Set (Set)
import Manticore.Formula

type KB a = Set (Formula a)
