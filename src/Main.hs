module Main where

import Cli.Input as Input
import Cli.Output as Output

import qualified Calculator.Calculator(calculateFromString)

main :: IO ()
main = do
    input <- Input.welcome "Введите выражение:"

    Output.tokens . calculateFromString $ input
