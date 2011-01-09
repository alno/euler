import Data.List

isPrime []    x                  = True
isPrime (h:t) x | h*h > x        = True
                | x `mod` h == 0 = False
                | otherwise      = isPrime t x

isPermutation x y = sx == sy where sx = sort (show x)
                                   sy = sort (show y)

primes = 2 : filter (isPrime primes) [3,5..]
primes4digits = dropWhile (< 1000) (takeWhile (<= 9999) primes)

seqs = [ [first, second, third] | first <- primes4digits, second <- primes4digits, second > first,
                                  let third = 2*second-first,
                                  isPermutation first second, isPermutation first third, isPrime primes third ]