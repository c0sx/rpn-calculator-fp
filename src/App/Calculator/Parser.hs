module App.Calculator.Parser(parse) where 

import Data.Char(isSpace, isNumber)

parse :: String -> [String]
parse = parse' . stripWhitespaces where
    stripWhitespaces :: String -> String
    stripWhitespaces = filter $ not . isSpace

    parse' :: String -> [String]
    parse' str = parseStr' str [] where 

        parseStr' :: String -> [String] -> [String]
        parseStr' [] s = s
        parseStr' (x:xs) s 
            | isNumber x = processNumber' x xs s
            | isMaybeUnary x = processUnaryOperator' x xs s
            | otherwise = parseStr' xs (s ++ [[x]])

        processNumber' :: Char -> String -> [String] -> [String]
        processNumber' x xs s = 
            let token = x : nextToken xs
                start = length token - 1
                end = length xs
            in parseStr' (substring start end xs) (s ++ [token]) where
                substring :: Int -> Int -> String -> String
                substring start end str = take (end - start) (drop start str)

                nextToken :: String -> String
                nextToken [] = []
                nextToken (c:xs) 
                    | isNumber c = c : nextToken xs 
                    | otherwise = []

        processUnaryOperator' :: Char -> String -> [String] -> [String]
        processUnaryOperator' x xs s = 
            if isNumeric prev 
            then parseStr' xs (s ++ [[x]])
            else if x == '-' 
                then processNumber' x xs s 
                else parseStr' xs s where
                    isNumeric :: String -> Bool
                    isNumeric [] = False
                    isNumeric (x:xs) 
                        | x == '-' || x == '+' = isNumeric xs
                        | isNumber x = all isNumber xs
                        | otherwise = False
                    
                    prev = if not (null s) then last s else ""

        isMaybeUnary :: Char -> Bool 
        isMaybeUnary x = x == '+' || x == '-'


