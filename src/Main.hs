module Main where

import Cli.Input
import Cli.Output

import qualified RpnExpression.Parser as RpnExpression
import qualified Lexer.InfixTokens as Infix

main :: IO ()
main = do
    input <- welcome "Введите выражение:"

    print . Infix.getTokens . Infix.createFromString $ input
