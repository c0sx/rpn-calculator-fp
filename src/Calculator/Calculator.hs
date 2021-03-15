module Calculator.Calculator where 

import Calculator.Parser(parse)
import Calculator.Tokenizer(tokenize, Token(..), toString)
import Calculator.SortingStation(transform)
import Calculator.Calculation(Calculation(..))

calculateFromString :: String -> Calculation
calculateFromString str = 
    let expression = transform . tokenize . parse $ str
        value = calculate expression

    in Calculation expression value

calculate :: [Token] -> Double  
calculate = head . foldl foldingFunction []  
    where   foldingFunction (x:y:ys) Multiply = (x * y):ys  
            foldingFunction (x:y:ys) Add = (x + y):ys  
            foldingFunction (x:y:ys) Subtract = (y - x):ys  
            foldingFunction (x:y:ys) Divide = (y / x):ys
            foldingFunction xs value = (read (toString value) :: Double):xs 