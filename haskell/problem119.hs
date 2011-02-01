import Char
import Data.List

sumOfDigits :: Show a => a -> Integer
sumOfDigits = toInteger . sum . (map digitToInt) . show

nums :: [Integer]
nums = (sort . nub) [m | n <- [2..100], k <- [1..50], let m = n^k, m > 10, sumOfDigits m == n]

main :: IO ()
main = print (nums !! 29)
