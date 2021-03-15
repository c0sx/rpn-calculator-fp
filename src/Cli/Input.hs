module Cli.Input (welcome) where

import Cli.Output(message)

welcome :: String -> IO String
welcome msg = do
    message msg
    getLine