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

sortedUnion [] l = l
sortedUnion l [] = l
sortedUnion a@(ah:at) b@(bh:bt) | ah == bh = ah:sortedUnion at bt
                                | ah <  bh = ah:sortedUnion at b
                                | ah >  bh = bh:sortedUnion a  bt

sortedConcat [] = []
sortedConcat (h:t) = sortedUnion h $ sortedConcat t

primes4 lim = sortedConcat [ primes3 (lim - p4*p4*p4*p4) p4 | p4 <- takeWhile (\p -> p*p*p*p < lim) primes ]
primes3 lim p4 = sortedConcat [ primes2 (lim - p3*p3*p3) p4 p3 | p3 <- takeWhile (\p -> p*p*p < lim) primes ]
primes2 lim p4 p3 = [ p2*p2 + p3*p3*p3 + p4*p4*p4*p4 | p2 <- takeWhile (\p -> p*p < lim) primes ]

numOfPrimes = length $ primes4 (5*10^7)

-- Main
main :: IO ()
main = print numOfPrimes
