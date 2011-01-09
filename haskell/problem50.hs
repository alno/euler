-- TODO: Automatically select limit`

import Data.Function
import Data.List

isPrime []    x                  = True
isPrime (h:t) x | h*h > x        = True
                | x `mod` h == 0 = False
                | otherwise      = isPrime t x

primes = 2 : filter (isPrime primes) [3,5..]

sumPrime from = sumPrimeAdd from (primes !! from,1) (primes !! from,1)

sumPrimeAdd from (s,l) best = let nextSum = primes !! (from+l) + s in if nextSum >= 1000000
                                  then best
                                  else if isPrime primes nextSum
                                       then sumPrimeAdd from (nextSum,l+1) (nextSum,l+1)
                                       else sumPrimeAdd from (nextSum,l+1) best

sumPrimes = [ sumPrime i | i <- [0..1000] ]
maxSumPrime = maximumBy (compare `on` snd) sumPrimes