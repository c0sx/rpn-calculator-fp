module App.Calculator.Parser(parse) where 

import Data.Char(isSpace, isNumber)

parse :: String -> [String]
parse = parse' . stripWhitespaces where
    stripWhitespaces :: String -> String
    stripWhitespaces = filter $ not . isSpace

    parse' :: String -> [String]
    parse' str = parseStr' str [] where 

        parseStr' :: String -> [String] -> [String]
        parseStr' [] parsed = parsed
        parseStr' (x:xs) parsed
            | isNumber x = processNumber' x xs parsed
            | isMaybeUnary x = processUnaryOperator' x xs parsed
            | otherwise = parseStr' xs (parsed ++ [[x]])

        processNumber' :: Char -> String -> [String] -> [String]
        processNumber' x xs parsed = 
            let token = x : nextToken xs
                start = length token - 1
                end = length xs
            in parseStr' (substring start end xs) (parsed ++ [token]) where
                substring :: Int -> Int -> String -> String
                substring start end str = take (end - start) (drop start str)

                nextToken :: String -> String
                nextToken [] = []
                nextToken (c:xs) 
                    | isNumber c = c : nextToken xs 
                    | otherwise = []

        processUnaryOperator' :: Char -> String -> [String] -> [String]
        processUnaryOperator' x xs parsed = 
            if isNumeric prev 
            then parseStr' xs (parsed ++ [[x]])
            else if x == '-' 
                then processNumber' x xs parsed 
                else parseStr' xs parsed where
                    isNumeric :: String -> Bool
                    isNumeric [] = False
                    isNumeric (x:xs) 
                        | x == '-' || x == '+' = isNumeric xs
                        | isNumber x = all isNumber xs
                        | otherwise = False
                    
                    prev = if not (null parsed) then last parsed else ""

        isMaybeUnary :: Char -> Bool 
        isMaybeUnary x = x == '+' || x == '-'


