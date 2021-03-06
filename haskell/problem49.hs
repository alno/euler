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

isPermutation x y = sx == sy where sx = sort (show x)
                                   sy = sort (show y)
primes4digits = dropWhile (< 1000) (takeWhile (<= 9999) primes)

seqs = [ [first, second, third] | first <- primes4digits, second <- primes4digits, second > first,
                                  let third = 2*second-first,
                                  isPermutation first second, isPermutation first third, isPrime third ]

otherSeq = last seqs
otherSeqConcat = foldl1 (++) (map show otherSeq)

-- Main
main :: IO ()
main = print otherSeqConcat
