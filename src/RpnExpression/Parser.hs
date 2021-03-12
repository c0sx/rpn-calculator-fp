module RpnExpression.Parser (parse) where

import Data.Char(isNumber)

sortingStation :: String -> String
sortingStation str = 
    let stack = []
    in unwords (foldl processToken stack str)

processToken :: [String] -> Char -> [String]
processToken stack token
    | isNumber token = stack ++ [[token]]
    | otherwise = stack

parse :: String -> String
parse = sortingStation