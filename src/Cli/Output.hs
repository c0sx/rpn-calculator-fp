module Cli.Output (message, tokens) where 

import Calculator.Tokenizer(Token(..))

message :: String -> IO ()
message = putStrLn

tokens :: [Token] -> IO ()
tokens = message .  toString

toString:: [Token] -> String
toString tokens = unwords (map showToken tokens)

showToken :: Token -> String
showToken = toStringOne where
    toStringOne (Number n) = show n
    toStringOne AddOp = "+"
    toStringOne MulOp = "*"
    toStringOne DivOp = "/"
    toStringOne SubOp = "-"
    toStringOne ParenOpen = "("
    toStringOne ParenClose = ")"