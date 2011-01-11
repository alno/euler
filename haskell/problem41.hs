-- GHC required: permutations

import Char
import Data.List

-- There are no prime pandigitals with n = 9 (all pandigitals with 9 digits as divisible by 3)
-- Last digit should be one of "1379"

allDigits = ['1'..'9']
lastDigits = "1379"

possiblePrimePandigitals :: Int -> [Integer]
possiblePrimePandigitals n = [ read (perm ++ [lastDigit]) | lastDigit <- plds, perm <- permutations (delete lastDigit pds) ] where pds = take n allDigits
                                                                                                                                   plds = takeWhile (<=(intToDigit n)) lastDigits

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : filter (noDivisors primes) [3,5..]

-- Function to check is the number is prime
isPrime = noDivisors primes

primePandigitals n = filter isPrime (possiblePrimePandigitals n)

maxPrimePandigital = maximum (concat (map primePandigitals [1..8]))
