module App.Calculator.Parser(parse) where 

import Data.Char(isSpace, isNumber)

parse :: String -> [String]
parse = parse' . stripWhitespaces

stripWhitespaces :: String -> String
stripWhitespaces = filter $ not . isSpace

parse' :: String -> [String]
parse' str = parseStr' str [] where 
    parseStr' [] s = s
    parseStr' (x:xs) s 
        | isNumber x = 
            let token = x : nextToken xs
                start = length token - 1
                end = length xs
            in parseStr' (substring start end xs) (s ++ [token])
        | isMaybeUnary x = 
            let prev = if length s > 0 then last s else ""
            in if isNumeric prev 
                then parseStr' xs (s ++ [[x]])
                else             
                    let token = x : nextToken xs
                        start = length token - 1
                        end = length xs
                    in parseStr' (substring start end xs) (s ++ [token])
        | otherwise = parseStr' xs (s ++ [[x]])

nextToken :: String -> String
nextToken [] = []
nextToken (c:xs) 
    | isNumber c = c : nextToken xs 
    | otherwise = []

isMaybeUnary :: Char -> Bool 
isMaybeUnary x = x == '+' || x == '-'

isNumeric :: String -> Bool
isNumeric [] = False
isNumeric (x:xs) 
    | x == '-' || x == '+' = isNumeric xs
    | isNumber x = all isNumber xs
    | otherwise = False

substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)