module App.Calculator.Calculation (Calculation(..), getExpression, getValue) where

import App.Calculator.Tokenizer(Token)

data Calculation = Calculation [Token] Double

getExpression :: Calculation -> [Token]
getExpression (Calculation expression _) = expression

getValue :: Calculation -> Double
getValue (Calculation _ value) = value