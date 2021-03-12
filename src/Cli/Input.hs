module Cli.Input (welcome) where

import Cli.Output as Output

welcome :: String -> IO String
welcome msg = do
    Output.message msg
    getLine