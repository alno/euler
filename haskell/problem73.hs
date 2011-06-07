import Ratio

-- c/d < a/b < e/d
-- c < a*d / b < e

maxDenominator = 12000

countBetween = length [ x | den <- [2..maxDenominator], num <- [den `div` 3 + 1..den `div` 2], let x = (num%den), numerator x == num, x > 1%3, x < 1%2 ]

main :: IO ()
main = print countBetween
