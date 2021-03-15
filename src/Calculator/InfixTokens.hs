module Calculator.InfixTokens (createFromString) where 

import Data.Char(isSpace, isNumber)

createFromString :: String -> [String]
createFromString = tokenize . stripWhitespaces

stripWhitespaces :: String -> String
stripWhitespaces = filter $ not . isSpace

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