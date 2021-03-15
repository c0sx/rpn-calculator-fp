module App.Calculator.Parser.Parse(parse) where 

import Data.Char(isSpace, isNumber)
import Debug.Trace

parse :: String -> [String]
parse = parse' . stripWhitespaces

stripWhitespaces :: String -> String
stripWhitespaces = filter $ not . isSpace

parse' :: String -> [String]
parse' [] = []
parse' (x:xs) 
    | isNumber x = 
        let token = x : nextToken xs
            start = length token - 1
            end = length xs
        in token : parse' (substring start end xs)
    | otherwise = [x] : parse' xs

nextToken :: String -> String
nextToken [] = []
nextToken (c:xs) 
    | isNumber c = c : nextToken xs 
    | isBracket c = 
        let next = head xs
        in if isUnaryOperator next then c : nextToken xs
        else []
    | otherwise = []

isBracket :: Char -> Bool
isBracket ')' = True
isBracket '(' = True
isBracket _ = False

isUnaryOperator :: Char -> Bool
isUnaryOperator '-' = True
isUnaryOperator '+' = True
isUnaryOperator _ = False

substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)
