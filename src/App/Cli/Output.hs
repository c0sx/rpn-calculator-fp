module App.Cli.Output (message, showCalculation) where 

import App.Calculator.Tokenizer(Token(..), toString)
import App.Calculator.Calculation(Calculation(..), getExpression, getValue)

message :: String -> IO ()
message = putStrLn

showCalculation :: Calculation -> IO ()
showCalculation result = do 
    putStrLn expression
    putStrLn value where
        value = show $ getValue result 
        expression = stringify $ getExpression result where
            stringify :: [Token] -> String
            stringify tokens = unwords (map toString tokens)
