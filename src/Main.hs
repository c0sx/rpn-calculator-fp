module Main where

import Cli.Input
import Cli.Output

import qualified RpnExpression.Parser as RpnExpression
import qualified Lexer.Parser as Lexer

main :: IO ()
main = do
    input <- welcome "Введите выражение:"

    writeMessage . RpnExpression.parse . Lexer.parseFromString $ input
