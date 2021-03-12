module Calculator.RpnTokens (RpnList, new, getTokens) where

import Data.Char(isNumber)

new :: [String] -> RpnList
new = RpnList

newtype RpnList = RpnList [String]

getTokens :: RpnList -> [String]
getTokens (RpnList tokens) = tokens