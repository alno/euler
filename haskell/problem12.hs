import Data.List

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

nDivsFrom c x y | m == 0 = nDivsFrom (c+1) d y
                | otherwise = (x,c)
    where (d,m) = divMod x y

nDivs = nDivsFrom 1

divisorCountFrom cnt (h:t) x | x == 1    = cnt
                             | h * h > x = cnt * 2
                             | otherwise = divisorCountFrom (cnt * c) t d where (d,c) = nDivs x h

divisorCount = divisorCountFrom 1 primes

triangleNumbers :: [Integer]
triangleNumbers = scanl1 (+) [1..]

triangleNumbersWithDivisors = map (\x -> (x, divisorCount x)) triangleNumbers

firstWithMoreThan500Divisors = find (\x -> snd x >= 500 ) triangleNumbersWithDivisors

triangleNumberFrom n = n * (n+1) `div` 2

numbersWithDivisors = map (\x -> (x, divisorCount x)) [5..]

firstWithMoreThan200Divisors = find (\x -> snd x >= 100 ) numbersWithDivisors