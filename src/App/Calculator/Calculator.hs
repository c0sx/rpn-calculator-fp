module App.Calculator.Calculator (calculateFromString) where 

import Debug.Trace(trace)
import Data.Maybe(fromMaybe)

import App.Calculator.Parser(parse)
import App.Calculator.Tokenizer(tokenize, Token(..), toString)
import App.Calculator.SortingStation(transform)
import App.Calculator.Calculation(Calculation(..))

calculateFromString :: String -> Calculation
calculateFromString input = calculate . transform . tokenize . parse $ input

calculate :: [Token] -> Calculation
calculate expression = Calculation expression value where 
    value = calculate' expression where
        calculate' :: [Token] -> Double
        calculate' = head . foldl foldingFunction [] where 
            foldingFunction (x:y:ys) Multiply = (x * y):ys  
            foldingFunction (x:y:ys) Add = (x + y):ys  
            foldingFunction (x:y:ys) Subtract = (y - x):ys  
            foldingFunction (x:y:ys) Divide = (y / x):ys
            foldingFunction xs value = tokenToDouble value:xs where 
                tokenToDouble :: Token -> Double
                tokenToDouble token = read (toString token) :: Double