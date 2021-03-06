-- Brute-force, need improvements

import Data.List

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime = noDivisors primes

conc a b = read (show a ++ show b)

isAddPrime [] p = True
isAddPrime (h:t) p = isPrime (conc p h) && isPrime (conc h p) && isAddPrime t p

extendPrimeSet smallSet = [ p:op | p <- primes, op <- takeWhile (\op -> p > head op) smallSet, isAddPrime op p ]

findPrimeSet 1 = map (\p -> [p]) primes
findPrimeSet n = extendPrimeSet $ findPrimeSet (n-1)

-- Main
main :: IO ()
main = print . sum . head $ findPrimeSet 5
