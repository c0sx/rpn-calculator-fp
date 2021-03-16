module App.Calculator.Tokenizer (tokenize, Token(..), toString) where 

import Data.Char(isNumber)

data Token = Number Int | OpenBracket | CloseBracket | Add | Multiply | Divide | Subtract 
    deriving (Show, Eq)

tokenize :: [String] -> [Token]
tokenize = map mapToken where
    mapToken :: String -> Token
    mapToken "+" = Add
    mapToken "-" = Subtract
    mapToken "*" = Multiply
    mapToken "/" = Divide
    mapToken "(" = OpenBracket
    mapToken ")" = CloseBracket
    mapToken s
        | isNumeric s = Number (read s :: Int)
        | otherwise = error "Недопустимый токен" where
            isNumeric :: String -> Bool
            isNumeric [] = False
            isNumeric (x:xs) 
                | x == '-' || x == '+' = isNumeric xs
                | isNumber x = all isNumber xs
                | otherwise = False
    
toString :: Token -> String
toString = toStringOne where
    toStringOne (Number n) = show n
    toStringOne Add = "+"
    toStringOne Multiply = "*"
    toStringOne Divide = "/"
    toStringOne Subtract = "-"
    toStringOne OpenBracket = "("
    toStringOne CloseBracket = ")"