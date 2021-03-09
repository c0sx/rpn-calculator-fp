module Cli.Output (writeMessage) where 

writeMessage :: String -> IO ()
writeMessage msg = putStrLn msg