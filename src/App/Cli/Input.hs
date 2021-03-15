module App.Cli.Input (welcome) where

import App.Cli.Output(message)

welcome :: String -> IO String
welcome msg = do
    message msg
    getLine