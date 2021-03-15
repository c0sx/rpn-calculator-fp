module App.Calculator.Calculator (calculateFromString) where 

import App.Calculator.Parser.Parse(parse)
import App.Calculator.Tokenizer(tokenize, Token(..), toString)
import App.Calculator.SortingStation(transform)
import App.Calculator.Calculation(Calculation(..))

calculateFromString :: String -> Calculation
calculateFromString str = 

    -- todo:
    -- строим AST по полученной строке
    -- обрабатываем унарные операторы как часть числа
    -- преобразуем AST в список токенов
    -- преобразуем в rpn
    -- вычисляем

    let expression = transform . tokenize . parse $ str
        value = calculate expression

    in Calculation expression value

calculate :: [Token] -> Double  
calculate = head . foldl foldingFunction []  
    where   foldingFunction (x:y:ys) Multiply = (x * y):ys  
            foldingFunction (x:y:ys) Add = (x + y):ys  
            foldingFunction (x:y:ys) Subtract = (y - x):ys  
            foldingFunction (x:y:ys) Divide = (y / x):ys
            foldingFunction xs value = tokenToDouble value:xs 

tokenToDouble :: Token -> Double
tokenToDouble token = read (toString token) :: Double