import Data.Ratio

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime = noDivisors primes

intSqrt = truncate . sqrt . fromInteger

ff m 0 = truncate m
ff m b | m < 1     = 0
       | otherwise = ff m (b-1) - ff (m / fromInteger pb) (b-1) where pb = primes !! (b-1)

primeCount 1 = 0
primeCount x = ff m n + n - 1
    where m = fromInteger x
          n = primeCount (intSqrt x)

-- Semi-primes
limit :: Integer
limit = 10^8-1

semiPrimes = [ sp | p1 <- takeWhile (< limit `div` 2) primes, p2 <- takeWhile (< limit `div` p1) primes, let sp = p1 * p2 ]

countSemiPrimes = sum [ primeCount (limit `div` p1) | p1 <- takeWhile (<= limit `div` 2) primes ]

main :: IO ()
main = print (primeCount (limit `div` 2))