import Data.List

isPrime []    x                  = True
isPrime (h:t) x | h*h > x        = True
                | x `mod` h == 0 = False
                | otherwise      = isPrime t x

rotate l i = (drop i l) ++ (take i l)

rotations l = [ rotate l i | i <- [1..length l - 1] ]

primes = 2 : filter (isPrime primes) [3,5..]

isCircularPrime x = all (isPrime primes) (map read (rotations (show x)))

circular_primes = filter isCircularPrime primes
circular_primes_less_million = filter isCircularPrime (takeWhile (<1000000) primes)