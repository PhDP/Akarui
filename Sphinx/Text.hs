module Sphinx.Text where

rmQuotes :: String -> String
rmQuotes = filter (/= '\"')

mkString :: [String] -> String
mkString = foldr1 (\t' acc -> t' ++ ", " ++ acc)
