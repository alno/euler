import Char
import Array

sumDigitSquares n = sum (map ((^2).digitToInt) (show n))

maxNum = 10^7-1
maxPre = 7*81

is89 n | n == 89   = True
       | n == 1    = False
       | otherwise = is89 $! sumDigitSquares n

is89Array = array (1,maxPre) [(i,is89 i) | i <- [1..maxPre]]

count = length (filter (\x -> is89Array ! sumDigitSquares x) [1..maxNum])

main :: IO ()
main = print count
