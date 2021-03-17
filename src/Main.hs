module Main where

import Data.Char(isNumber)

import App.Cli.Input(welcome)
import App.Cli.Output(showCalculation, showError)
import App.Calculator.Calculator(calculateFromString)


main :: IO ()
main = do
    input <- welcome "Введите выражение:"

    case calculateFromString input of
        Right result -> showCalculation result
        Left err -> showError err