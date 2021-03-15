module Calculator.Tokenizer (fromInfixTokens, Token(..)) where 

import Data.Char(isNumber)

data Token = Number Int | ParenOpen | ParenClose | AddOp | MulOp  | DivOp | SubOp 
    deriving (Show, Eq)

fromInfixTokens :: [String] -> [Token]
fromInfixTokens = map mapToken

mapToken :: String -> Token
mapToken "+" = AddOp
mapToken "-" = SubOp
mapToken "*" = MulOp
mapToken "/" = DivOp
mapToken "(" = ParenOpen
mapToken ")" = ParenClose
mapToken s
    | isNumeric s = Number (read s :: Int)
    | otherwise = error "Недопустимый токен"

isNumeric :: String -> Bool
isNumeric = all isNumber
