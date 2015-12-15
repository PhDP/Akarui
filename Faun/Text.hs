-- | Useful (and private) functions to handle text.
module Faun.Text where

import qualified Data.Text as T

-- | Removes quotation marks from a string.
rmQuotes :: T.Text -> T.Text
rmQuotes = T.filter (/= '\"')

-- | Builds a string with ", " between all elements.
mkString :: [T.Text] -> T.Text
mkString = foldr1 (\x acc -> T.concat [x, ", ", acc])

-- | Surrounds the string if b is true (used to print formulas).
surrIf :: Bool -> T.Text -> T.Text
surrIf b txt = if b then T.concat ["(", txt, ")"] else txt

-- | Adds brackets arround a string.
addBrackets :: T.Text -> T.Text
addBrackets s = T.concat ["{", s, "}"]
