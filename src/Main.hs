module Main where

import Data.Char(isNumber)

import App.Cli.Input(welcome)
import App.Cli.Output(showCalculation)
import App.Calculator.Calculator(calculateFromString)

main :: IO ()
main = do
    input <- welcome "Введите выражение:"

    showCalculation . calculateFromString $ input
