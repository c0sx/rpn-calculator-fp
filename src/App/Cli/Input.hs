module App.Cli.Input (welcome) where

import App.Cli.Output(message)

welcome :: String -> IO String
welcome message = do
    message message
    getLine