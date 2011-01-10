import Data.List

divisorCountFrom :: Integer -> Int -> Integer -> Int
divisorCountFrom i s x | i*i >  x       = s
                       | i*i == x       = s + 1
                       | otherwise      = let (d,m) = divMod x i in
                                            if m == 0 then divisorCountFrom (i+1) (s+2) x
                                                      else divisorCountFrom (i+1)  s    x

divisorCount = divisorCountFrom 2 2

triangleNumbers :: [Integer]
triangleNumbers = scanl1 (+) [1..]

triangleNumbersWithDivisors = map (\x -> (x, divisorCount x)) triangleNumbers

firstWithMoreThan500Divisors = find (\x -> snd x >= 500 ) triangleNumbersWithDivisors