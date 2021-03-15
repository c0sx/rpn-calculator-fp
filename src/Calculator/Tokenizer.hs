module Calculator.Tokenizer (tokenize, Token(..), toString) where 

import Data.Char(isNumber)

data Token = Number Int | OpenBracket | CloseBracket | Add | Multiply | Divide | Subtract 
    deriving (Show, Eq)

tokenize :: [String] -> [Token]
tokenize = map mapToken

mapToken :: String -> Token
mapToken "+" = Add
mapToken "-" = Subtract
mapToken "*" = Multiply
mapToken "/" = Divide
mapToken "(" = OpenBracket
mapToken ")" = CloseBracket
mapToken s
    | isNumeric s = Number (read s :: Int)
    | otherwise = error "Недопустимый токен"

isNumeric :: String -> Bool
isNumeric = all isNumber
    
toString :: Token -> String
toString = toStringOne where
    toStringOne (Number n) = show n
    toStringOne Add = "+"
    toStringOne Multiply = "*"
    toStringOne Divide = "/"
    toStringOne Subtract = "-"
    toStringOne OpenBracket = "("
    toStringOne CloseBracket = ")"