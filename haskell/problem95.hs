-- Brute-force solution, needs improvements

import Data.Function
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

primeFactorsExpApp factors (h:t) x | h > x              = factors
                                   | h * h > x          = ((x,1):factors)
                                   | otherwise          = primeFactorsExpApp ((h,c):factors) t d where (d,c) = nDivs x h

primeFactorsExp = primeFactorsExpApp [] primes

sumOf :: [(Integer,Integer)] -> Integer
sumOf []        = 1
sumOf ((p,q):t) =  (sumOf t) * (p ^ (q+1) - 1) `div` (p-1)

sumOfDivisors n = sumOf (primeFactorsExp n) - n

mx = 1000000

chainLengthF f c n | k >  mx     = Nothing
                   | f >  k      = Nothing
                   | f == k      = Just (length c)
                   | elem k c    = Nothing
                   | otherwise   = chainLengthF f (k:c) k
    where k = sumOfDivisors n

chainLength n = chainLengthF n [n] n

longestChain = maximumBy (compare `on` snd) [ (n,l) | n <- [1..10^6], let ll = chainLength n, ll /= Nothing, let Just l = ll ]

main :: IO ()
main = print longestChain
