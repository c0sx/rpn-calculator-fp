module Lexer.Parser (parseFromString) where 

import Data.Char(isSpace, isNumber)

stripWhitespaces :: String -> String
stripWhitespaces str = filter (\x -> isSpace x == False) str

parseFromString :: String -> String
parseFromString str = stripWhitespaces $ str