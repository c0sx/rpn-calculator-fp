module Calculator.Calculator where 

import qualified Calculator.InfixTokens as Infix
import qualified Calculator.Tokenizer as Tokenizer
import qualified Calculator.SortingStation as Station
import Calculator.Calculation(Calculation)
import Calculator.Tokenizer(Token)

-- calculateFromString :: String -> Calculation
calculateFromString :: String -> [Token]
calculateFromString str =
    -- calculate . transform . tokenize . parse
    Station.transform . Tokenizer.fromInfixTokens . Infix.createFromString

