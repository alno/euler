import Char
import List

fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

f9 = fact 9

digits n = map digitToInt (show n)

sumOfFactDigits n = sum (map fact (digits n))

specialNums = [ x | x <- [1..7*f9], x == sumOfFactDigits x]

numsFor l n f | n * 10 > f + f9 = []
              | n * 10 + 9 < f + 1  = []
              | otherwise = [ nn | d <- [0..9], let nn = n * 10 + d, f + fact d == nn ] ++ concat [ numsFor (l+1) nn nf | d <- [0..9], let nn = n * 10 + d, let nf = f + fact d ]

nums = concat [ numsFor 1 d (fact d) | d <- [1..9]]

main :: IO ()
main = print nums
