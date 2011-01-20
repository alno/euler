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

rotate l i = (drop i l) ++ (take i l)

rotations l = [ rotate l i | i <- [1..length l - 1] ]

isCircularPrime x = all isPrime (map read (rotations (show x)))

circular_primes = filter isCircularPrime primes
circular_primes_less_million = filter isCircularPrime (takeWhile (<1000000) primes)

-- Main
main :: IO ()
main = print (length circular_primes_less_million)
