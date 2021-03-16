module App.Calculator.SortingStation (transform) where 

import Data.Char(isNumber)

import App.Calculator.Tokenizer(Token(..))

transform :: [Token] -> [Token]
transform tokens = transform' tokens [] [] where 
    transform' [] [] q = q
    transform' [] s q =
        if head s == OpenBracket
        then error "Синтаксическая ошибка"
        else transform' [] (tail s) (q ++ [head s])
    transform' (x:xs) s q = case x of 
        Number n -> transform' xs s (q ++ [Number n])
        OpenBracket -> transform' xs (OpenBracket : s) q
        CloseBracket -> transform' xs s0 q0 where
            s0 = tail $ dropWhile (/= OpenBracket) s
            q0 = q ++ takeWhile (/= OpenBracket) s
        o1 -> transform' xs s1 q1 where
            cond o2 = isOperator o2 && (priority o1 < priority o2)
            spl = span cond s
            s1 = o1 : snd spl
            q1 = q ++ fst spl 
             
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