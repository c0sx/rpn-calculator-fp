module Lexer.Parser (parseFromString) where 

import Data.Char(isSpace, isNumber)
import Data.List

stripWhitespaces :: String -> String
stripWhitespaces = filter (\ x -> not (isSpace x))

tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs) 
    | isNumber x = 
        let token = x : nextToken xs
        in token : tokenize (substring (length token - 1) (length xs) xs)
    | otherwise = [x] : tokenize xs

nextToken :: String -> String
nextToken [] = []
nextToken (c:xs) 
    | isNumber c = c : nextToken xs 
    | otherwise = []

substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)

parseFromString :: String -> [String]
parseFromString = tokenize . stripWhitespaces