module Lexer.InfixTokens (createFromString, getTokens) where 

import Data.Char(isSpace, isNumber)
import Data.List

stripWhitespaces :: String -> String
stripWhitespaces = filter (\ x -> not (isSpace x))

createFromString :: String -> InfixList
createFromString = InfixList . stripWhitespaces

newtype InfixList = InfixList String

value :: InfixList -> String
value (InfixList value) = value

getTokens :: InfixList -> [String]
getTokens = tokenize . value

tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs) 
    | isNumber x = 
        let token = x : nextToken xs
            start = length token - 1
            end = length xs
        in token : tokenize (substring start end xs)
    | otherwise = [x] : tokenize xs

nextToken :: String -> String
nextToken [] = []
nextToken (c:xs) 
    | isNumber c = c : nextToken xs 
    | otherwise = []

substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)