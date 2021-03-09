module Main where

import Cli.Input
import Cli.Output

main :: IO ()
main = do
    input <- welcome "Введите выражение:"
    writeMessage ("Инфиксное выражение: " ++ input)
