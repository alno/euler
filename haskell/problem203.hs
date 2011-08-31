import Data.List

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

nDivsFrom c x y | m == 0 = nDivsFrom (c+1) d y
                | otherwise = (x,c)
    where (d,m) = divMod x y

nDivs = nDivsFrom 0

-- Prime divisors

primeFactorsExpApp factors (h:t) x | h > x              = factors
                                   | h * h > x          = ((x,1):factors)
                                   | otherwise          = primeFactorsExpApp ((h,c):factors) t d where (d,c) = nDivs x h

primeFactorsExp = primeFactorsExpApp [] primes

-- Square-free check

isSquareFreeFactors []        = True
isSquareFreeFactors ((_,c):t) = c < 2 && isSquareFreeFactors t

isSquareFree = isSquareFreeFactors . primeFactorsExp

-- Binomial coefficients

factFor l n | l >= n = 1
            | otherwise = n * factFor l (n-1)

fact = factFor 1

coef n k = factFor (n-k) n `div` fact k

coefs = 1:nub [ coef n k | n <-[1..50], k <- [1..n `div` 2] ]

-- Square-free coefficients

squareFree = filter isSquareFree coefs

sumSquareFree = sum squareFree

main :: IO ()
main = print sumSquareFree
