{-# LANGUAGE OverloadedStrings #-}

-- | Useful functions to handle text.
module Sphinx.Text where

-- | Removes quotation marks from a string.
rmQuotes :: String -> String
rmQuotes = filter (/= '\"')

-- | Builds a string with ", " between all elements.
mkString :: [String] -> String
mkString = foldr1 (\x acc -> x ++ ", " ++ acc)
