module App.Cli.Output (message, showCalculation) where 

import App.Calculator.Tokenizer(Token(..), toString)
import App.Calculator.Calculation(Calculation(..), getExpression, getValue)

message :: String -> IO ()
message = putStrLn

showCalculation :: Calculation -> IO ()
showCalculation result = 
    let expression = stringify $ getExpression result
        value = show $ getValue result
    in do 
        putStrLn expression
        putStrLn value

stringify:: [Token] -> String
stringify tokens = unwords (map toString tokens)