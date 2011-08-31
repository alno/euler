import Data.List
import Data.Function

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime x = x > 1 && noDivisors primes x

numOfPrimesFrom n a b | isPrime (n*n + n*a + b) = numOfPrimesFrom (n+1) a b
                      | otherwise = n
numOfPrimes = numOfPrimesFrom 0

primesLess1000 = takeWhile (<1000) primes

tuples = [ (a*b,count) | a <- [-1000..1000], b <- primesLess1000, let count = numOfPrimes a b, count > 0 ]

maxTuple = maximumBy (compare `on` snd) tuples

main :: IO ()
main = print maxTuple
