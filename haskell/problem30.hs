import Char

sumOfDigitPowers n x = sum (map ((^n) . digitToInt) (show x))

isSumOfDigitPowers n x = x == sumOfDigitPowers n x

max4 :: Int
max4 = 4*9^4

max5 :: Int
max5 = 5*9^5

numbers = filter (isSumOfDigitPowers 5) [2..max5]

sumOfNumbers = sum numbers

main :: IO ()
main = print sumOfNumbers
