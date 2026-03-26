import Data.Char (isDigit, toLower)

variant :: Int
variant = 2

strIsDigit :: String -> [Bool]
strIsDigit s = map isDigit s

strToLower :: String -> String
strToLower s = map toLower s

answer :: String -> ([Bool], String)
answer s = (strIsDigit s, strToLower s)