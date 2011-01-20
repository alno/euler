import Data.List

fibNumbers = 0 : 1 : zipWith (+) fibNumbers (tail fibNumbers)

digits = ['1'..'9']

isPandigital x = sort x == digits

isEndsPandigital x = isPandigital (take 9 s) && isPandigital (drop (length s - 9) s) where s = show x

firstEndsPandigital = findIndex isEndsPandigital fibNumbers

-- Main
main :: IO ()
main = print "Not completed"
