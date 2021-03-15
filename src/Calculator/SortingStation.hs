module Calculator.SortingStation (transform) where 

import Data.Char(isNumber)

import Calculator.Tokenizer(Token(..))

isOp:: Token -> Bool
isOp AddOp = True
isOp MulOp = True
isOp DivOp = True
isOp SubOp = True
isOp _ = False

prec:: Token -> Int
prec AddOp = 0
prec SubOp = 0
prec MulOp = 1
prec DivOp = 1
prec ParenOpen = 2
prec ParenClose = 2
prec (Number _) = 3

transform :: [Token] -> [Token]
transform tokens = transform' tokens [] [] where 
    transform' [] [] q = q
    transform' [] s q =
        if head s == ParenOpen
        then error "Mismatched Parentheses"
        else transform' [] (tail s) (q ++ [head s])
    transform' (x:xs) s q = case x of 
        Number n -> transform' xs s (q ++ [Number n])
        ParenOpen -> transform' xs (ParenOpen:s) q
        ParenClose -> transform' xs s0 q0 where
            s0 = tail $ dropWhile (/= ParenOpen) s
            q0 = q ++ takeWhile (/= ParenOpen) s
        o1 -> transform' xs s1 q1 where
            cond o2 = isOp o2 && (prec o1 < prec o2)
            spl = span cond s
            s1 = o1 : snd spl
            q1 = q ++ fst spl