module Main where

import Cli.Input
import Cli.Output

import Lexer.Parser

main :: IO ()
main = do
    input <- welcome "Введите выражение:"
    stripped = stripWhitespaces input 
    writeMessage ("Инфиксное выражение: " ++ input)
