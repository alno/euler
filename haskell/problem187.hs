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

primesIn from to = takeWhile (<=to) (dropWhile (<=from) primes)

-- Prime counting function

intSqrt = truncate . sqrt . fromInteger
intTrrt x = truncate (exp (log (fromInteger x) / 3 ))

p2 m y = sum [ primeCount (m `div` p) - primeCount p + 1 | p <- primesIn y sm ] where sm = intSqrt m

ff m 0 = truncate m
ff m b | m < 1     = 0
       | otherwise = ff m (b-1) - ff (m / fromInteger pb) (b-1) where pb = primes !! (b-1)

primeCount 1 = 0
primeCount 2 = 1
primeCount 3 = 2
primeCount x = ff m n + n - 1 - p2 x y
    where m = fromInteger x
          y = intTrrt x + 1
          n = primeCount y

-- Semi-primes
limit :: Integer
limit = 10^8-1

semiPrimes = [ sp | p1 <- takeWhile (<= intSqrt limit) primes, p2 <- primesIn (p1-1) (limit `div` p1), let sp = p1 * p2 ]

countSemiPrimes = sum (zipWith (-) [ primeCount (limit `div` p1) | p1 <- takeWhile (<= intSqrt limit) primes ] [0..])

-- Main

main :: IO ()
main = print countSemiPrimes
