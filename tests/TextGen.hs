module TextGen where

import Test.QuickCheck
import Data.Char (isLower, isUpper)

genLowerChar :: Gen Char
genLowerChar = elements ['a'..'z']

genUpperChar :: Gen Char
genUpperChar = elements ['A'..'Z']

genPascalChar :: Gen Char
genPascalChar = oneof [genLowerChar, genUpperChar]

genLetterString :: Gen String
genLetterString = listOf genPascalChar

-- A string of letters
genPascalString :: Gen String
genPascalString = suchThat genLetterString (isUpper . head)

-- A string of letters starting with a lowercase letters
genCamelString :: Gen String
genCamelString = suchThat genLetterString (isLower . head)
