import Char

sumOfDigits x = sum [ digitToInt d | d <- show x ]

sumOfDigits2_1000 = sumOfDigits (2^1000)

-- Main
main :: IO ()
main = print sumOfDigits2_1000
