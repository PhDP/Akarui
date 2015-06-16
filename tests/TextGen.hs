module TextGen where

import Test.QuickCheck
import Data.Char (isLower, isUpper)

genLowerChar, genUpperChar :: Gen Char
genLowerChar = elements ['a'..'z']
genUpperChar = elements ['A'..'Z']

-- A lower or upper-case character.
genPascalChar :: Gen Char
genPascalChar = oneof [genLowerChar, genUpperChar]

-- A string of upper and lower-case characters.
genLetterString :: Gen String
genLetterString = listOf genPascalChar

-- Generetate non-null strings with a restriction on the first letter.
genStringFst :: (Char -> Bool) -> Gen String
genStringFst f = suchThat genLetterString (\s -> not (null s) && f (head s))

-- A string in Pascal format.
genPascalString :: Gen String
genPascalString = genStringFst isUpper

-- A string in camel format.
genCamelString :: Gen String
genCamelString = genStringFst isLower
