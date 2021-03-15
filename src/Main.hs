module Main where

import Cli.Input as Input
import Cli.Output(showCalculation)
import Calculator.Calculator(calculateFromString)

main :: IO ()
main = do
    input <- Input.welcome "Введите выражение:"

    showCalculation . calculateFromString $ input
