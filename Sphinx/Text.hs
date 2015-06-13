module Sphinx.Text where

rmQuotes :: String -> String
rmQuotes = filter (\c -> c /= '\"')
