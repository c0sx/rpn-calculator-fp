module Cli.Input (welcome) where

import Cli.Output

welcome :: String -> IO String
welcome msg = do
    writeMessage msg
    getLine