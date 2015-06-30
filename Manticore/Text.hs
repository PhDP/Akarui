{-# LANGUAGE OverloadedStrings #-}

-- | Useful (and private) functions to handle text.
module Manticore.Text where

-- | Removes quotation marks from a string.
rmQuotes :: String -> String
rmQuotes = filter (/= '\"')

-- | Builds a string with ", " between all elements.
mkString :: [String] -> String
mkString = foldr1 (\x acc -> x ++ ", " ++ acc)

-- | Surrounds the string if b is true (used to print formulas).
surrIf :: Bool -> String -> String
surrIf b str = if b then ('(' : str) ++ ")" else str

-- | Adds brackets arround a string
addBrackets :: String -> String
addBrackets s = ('{' : s) ++ "}"

-- | Surrounds a list.
surround :: a -> [a] -> [a]
surround e ls = (e : ls) ++ [e]

-- | Drops the last two characters of a list.
dropLst2 :: [a] -> [a]
dropLst2 = init . init
