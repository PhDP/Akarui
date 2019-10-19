module Faun.FOL.Parser.Bool
( parseBool
, getBool
, getTop
, getBot
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Faun.FOL.Parser.Core

parseBool :: String -> Either ParseError Bool
parseBool = parse (contents getBool) "<stdin>"

getBool, getTop, getBot :: Parser Bool
getBool = try getTop <|> getBot
getTop  = reservedOps ["True", "TRUE", "true", "Top", "T", "⊤"] >> return True
getBot  = reservedOps ["False", "FALSE", "false", "F", "Bottom", "Bot", "⊥"] >> return False
