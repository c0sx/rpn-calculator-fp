module Calculator.SortingStation (transform) where 

import Data.Char(isNumber)

import Calculator.Tokenizer(Token(..))

isOperator:: Token -> Bool
isOperator Add = True
isOperator Multiply = True
isOperator Divide = True
isOperator Subtract = True
isOperator _ = False

prec:: Token -> Int
prec Add = 0
prec Subtract = 0
prec Multiply = 1
prec Divide = 1
prec OpenBracket = 2
prec CloseBracket = 2
prec (Number _) = 3

transform :: [Token] -> [Token]
transform tokens = transform' tokens [] [] where 
    transform' [] [] q = q
    transform' [] s q =
        if head s == OpenBracket
        then error "Mismatched Parentheses"
        else transform' [] (tail s) (q ++ [head s])
    transform' (x:xs) s q = case x of 
        Number n -> transform' xs s (q ++ [Number n])
        OpenBracket -> transform' xs (OpenBracket : s) q
        CloseBracket -> transform' xs s0 q0 where
            s0 = tail $ dropWhile (/= OpenBracket) s
            q0 = q ++ takeWhile (/= OpenBracket) s
        o1 -> transform' xs s1 q1 where
            cond o2 = isOperator o2 && (prec o1 < prec o2)
            spl = span cond s
            s1 = o1 : snd spl
            q1 = q ++ fst spl