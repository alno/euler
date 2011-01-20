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

nDivs = nDivsFrom 0

divisorCountFrom cnt (h:t) x | x == 1    = cnt
                             | h * h > x = cnt * 2
                             | otherwise = divisorCountFrom (cnt * (c + 1)) t d where (d,c) = nDivs x h

divisorCount = divisorCountFrom 1 primes

appendn x n l | n == 0 = l
              | n >  0 = appendn x (n-1) (x:l)

primeFactorsDupApp factors (h:t) x | h > x              = factors
                                   | h * h > x          = (x:factors)
                                   | otherwise          = primeFactorsDupApp (appendn h c factors) t d where (d,c) = nDivs x h

primeFactorsDup = primeFactorsDupApp [] primes

minNumberWithDivisorCount n = product (zipWith (^) primes (map pred (primeFactorsDup n)))

triangleNumbers :: [Integer]
triangleNumbers = scanl1 (+) [1..]

triangleNumbersWithDivisors = map (\x -> (x, divisorCount x)) triangleNumbers

firstWithMoreThan500Divisors = find (\x -> snd x >= 500 ) triangleNumbersWithDivisors

-- Using imporvement from overview:

divisorCountForTriangle n | n `mod` 2 == 0 = divisorCount (n `div` 2) * divisorCount (n+1)
                          | otherwise = divisorCount n * divisorCount ((n+1) `div` 2)

firstWithMoreThan500Divisors2 = find (\x -> snd x >= 500 ) (map (\n -> (n*(n+1) `div` 2, divisorCountForTriangle n)) [1..])


-- Main
main :: IO ()
main = print firstWithMoreThan500Divisors2
