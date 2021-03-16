module Main where

import Data.Char(isNumber)

import App.Cli.Input(welcome)
import App.Cli.Output(showCalculation)
import App.Calculator.Calculator(calculateFromString)

main :: IO ()
main = 
    -- input <- welcome "Введите выражение:"

    print $ parseStr getInput

getInput :: String
getInput = "(10 - 5)"

parseStr :: String -> [String]
parseStr str = parseStr' str [] where 
    parseStr' [] s = s
    parseStr' (x:xs) s 
        | isNumber x = 
            -- собрать число
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
