module App.Calculator.Calculator (calculateFromString) where 

import Data.Either(Either(Right, Left))
import Debug.Trace(trace)

import App.Calculator.Parser(parse)
import App.Calculator.Tokenizer(tokenize, Token(..), toString)
import App.Calculator.SortingStation(transform)
import App.Calculator.Calculation(Calculation(..))

calculateFromString :: String -> Either String Calculation
calculateFromString input = case transform . tokenize . parse $ input of 
    Right tokens -> Right (calculate tokens)
    Left err -> Left err
        

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