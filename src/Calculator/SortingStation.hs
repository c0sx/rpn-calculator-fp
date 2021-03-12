module Calculator.SortingStation (sort) where 

import Data.Char(isNumber)

import Calculator.InfixTokens as Infix
import Calculator.RpnTokens as Rpn

sort :: Infix.InfixList -> Rpn.RpnList
sort = processTokens . Infix.getTokens

processTokens :: [String] -> Rpn.RpnList
processTokens = Rpn.new
