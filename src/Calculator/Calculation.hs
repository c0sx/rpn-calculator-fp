module Calculator.Calculation (Calculation) where

import Calculator.Tokenizer(Token)

data Calculation = Calculation {
    expression :: [Token],
    result :: Double
}