module Cli.Output (message, tokens) where 

message :: String -> IO ()
message = putStrLn

tokens :: [String] -> IO ()
tokens = message . unwords