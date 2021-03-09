module Lexer.Parser (stripWhitespaces) where 
    
import Data.Char(isSpace)

stripWhitespaces = map (dropWhile isSpace)