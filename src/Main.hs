module Main where

import Cli.Input(welcome)
import Cli.Output(showCalculation)
import Calculator.Calculator(calculateFromString)

main :: IO ()
main = do
    input <- welcome "Введите выражение:"

    showCalculation . calculateFromString $ input
