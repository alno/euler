-- Need optimizations

import Data.List

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime x = noDivisors primes x

nDivsFrom c x y | m == 0 = nDivsFrom (c+1) d y
                | otherwise = (x,c)
    where (d,m) = divMod x y

nDivs = nDivsFrom 0

-- Count of coprime to n numbers less than n (http://en.wikipedia.org/wiki/Euler%27s_totient_function)
coprimeCount' res (h:t) x | h > x              = res
                          | h * h > x          = res * (x-1)
                          | otherwise          = let (d,m) = divMod x h in
                                                     if  m == 0 then let (d,c) = nDivs x h in (coprimeCount' $! (res * (h-1) * h ^ (c-1))) t d
                                                                else coprimeCount' res t x

coprimeCount = coprimeCount' 1 primes

isPerm x y = sort (show x) == sort (show y)

minRatioN' bn br 2        = bn
minRatioN' bn br n        = if (r < br) && (isPerm n c) then minRatioN' n  r  (n-1)
                                                           else minRatioN' bn br (n-1)
  where c = coprimeCount n
        r = fromIntegral n / fromIntegral c

minRatioN = minRatioN' 0 (10^7) (10^7)

main = print minRatioN
