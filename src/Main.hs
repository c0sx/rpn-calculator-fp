module Main where

import Data.Char(isNumber)

import App.Cli.Input(welcome)
import App.Cli.Output(showCalculation)
import App.Calculator.Parser(parse)
import App.Calculator.Tokenizer(tokenize, Token(..), toString)
import App.Calculator.SortingStation(transform)
import App.Calculator.Calculator(calculate)
import App.Calculator.Calculation(Calculation(..))

main :: IO ()
main = do
    input <- welcome "Введите выражение:"

    showCalculation . calculate . transform . tokenize . parse $ input