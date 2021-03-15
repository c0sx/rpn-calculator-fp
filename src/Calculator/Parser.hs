module Calculator.Parser (parse) where 

import Data.Char(isSpace, isNumber)

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
    | otherwise = []

substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)