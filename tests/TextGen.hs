module TextGen where

import Test.QuickCheck

genLowerChar :: Gen Char
genLowerChar = elements ['a'..'z']

genUpperChar :: Gen Char
genUpperChar = elements ['A'..'Z']

genPascalChar :: Gen Char
genPascalChar = oneof [genLowerChar, genUpperChar]

genPascalString :: Gen String
genPascalString = listOf genPascalChar
