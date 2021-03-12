module Main where

import Cli.Input as Input
import Cli.Output as Output

import qualified Calculator.InfixTokens as Infix
import qualified Calculator.RpnTokens as Rpn
import qualified Calculator.SortingStation as Station

main :: IO ()
main = do
    input <- Input.welcome "Введите выражение:"

    Output.tokens . Rpn.getTokens . Station.sort . Infix.createFromString $ input
