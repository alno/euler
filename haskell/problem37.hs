import Data.List

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

primes2digits = (takeWhile (<100) (dropWhile (<10) primes))

-- Function to check is the number is prime
isPrime x = x > 1 && noDivisors primes x

isTruncatablePrimeLS s = a == [] || isPrime (read a) && isTruncatablePrimeLS a where a = tail s
isTruncatablePrimeRS s = b == [] || isPrime (read b) && isTruncatablePrimeRS b where b = init s

isPrimeL x = x > 1 && noDivisors (3:drop 3 primes) x

nextTruncatablePrimesSerieL serie = [ xl | d <- ['1'..'9'], x <- serie, let xs = show x, let xls = d:xs, let xl = read xls, isPrimeL xl ]
nextTruncatablePrimesSerieR serie = [ xl | x <- serie, let xs = show x, d <- "1379", let xls = xs++[d], let xl = read xls, isPrime xl ]

truncatablePrimesBy next check = takeWhile (/=[]) (iterate next (filter (check.show) primes2digits))

truncatablePrimesL = truncatablePrimesBy nextTruncatablePrimesSerieL isTruncatablePrimeLS
truncatablePrimesR = truncatablePrimesBy nextTruncatablePrimesSerieR isTruncatablePrimeRS

intersectSorted [] _ = []
intersectSorted _ [] = []
intersectSorted l1@(h1:t1) l2@(h2:t2) | h1 < h2  = intersectSorted t1 l2
                                      | h1 > h2  = intersectSorted l1 t2
                                      | h1 == h2 = h1:intersectSorted t1 t2

truncatablePrimes = concat (zipWith intersectSorted truncatablePrimesL truncatablePrimesR)
sumTruncatablePrimes = sum truncatablePrimes

-- Main
main :: IO ()
main = print sumTruncatablePrimes
