-- Brute-force solution, need imporvements

import Data.List

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : filter (noDivisors primes) [3,5..]

-- Function to check is the number is prime
isPrime = noDivisors primes

primeFactorsApp factors (h:t) x | length factors > 4 = Nothing
                                | h == x             = Just (h:factors)
                                | h > x              = Just factors
                                | otherwise          = let (d,m) = divMod x h in
                                                            if  m == 0 then primeFactorsApp (h:factors) t d
                                                                       else primeFactorsApp factors t x

primeFactors = primeFactorsApp [] primes

hasNPrimeFactorsApp fc (h:t) n x | fc > n             = False
                                 | h == x             = fc + 1 == n
                                 | h > x              = fc == n
                                 | otherwise          = let (d,m) = divMod x h in
                                                            if  m == 0 then hasNPrimeFactorsApp (fc+1) t n d
                                                                       else hasNPrimeFactorsApp fc t n x

hasNPrimeFactors = hasNPrimeFactorsApp 0 primes

nums = filter (hasNPrimeFactors 4) [4..]

firstFourFor (n1:t@(n2:n3:n4:_)) | n2 == n1 + 1 && n3 == n2 + 1 && n4 == n3 + 1 = n1
                                 | otherwise = firstFourFor t

firstFour = firstFourFor nums
