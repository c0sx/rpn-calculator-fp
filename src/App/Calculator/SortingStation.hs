module App.Calculator.SortingStation (transform) where 

import Data.Char(isNumber)
import Data.Either(Either(Right, Left))

import App.Calculator.Tokenizer(Token(..))

transform :: [Token] -> Either String [Token]
transform tokens = transform' tokens [] [] where 
    transform' [] [] queue = Right queue
    transform' [] stack queue =
        if head stack == OpenBracket
        then Left "Синтаксическая ошибка"
        else transform' [] (tail stack) (queue ++ [head stack])
    transform' (x:xs) stack queue = case x of 
        Number n -> transform' xs stack (queue ++ [Number n])
        OpenBracket -> transform' xs (OpenBracket : stack) queue
        CloseBracket -> transform' xs stack0 queue0 where
            stack0 = tail $ dropWhile (/= OpenBracket) stack
            queue0 = queue ++ takeWhile (/= OpenBracket) stack
        operator1 -> transform' xs stack1 queue1 where
            cond operator2 = isOperator operator2 && (priority operator1 < priority operator2)
            spl = span cond stack
            stack1 = operator1 : snd spl
            queue1 = queue ++ fst spl 
             
            isOperator:: Token -> Bool
            isOperator Add = True
            isOperator Multiply = True
            isOperator Divide = True
            isOperator Subtract = True
            isOperator _ = False

            priority:: Token -> Int
            priority Add = 0
            priority Subtract = 0
            priority Multiply = 1
            priority Divide = 1
            priority OpenBracket = 2
            priority CloseBracket = 2
            priority (Number _) = 3