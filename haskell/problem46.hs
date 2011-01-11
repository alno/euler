noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime = noDivisors primes

isSumOfPrimeAndSquareFrom i n | ds >= n        = False
                              | isPrime (n-ds) = True
                              | otherwise      = isSumOfPrimeAndSquareFrom (i+1) n
    where ds = 2 * i * i

isSumOfPrimeAndSquare = isSumOfPrimeAndSquareFrom 0
notSumOfPrimeAndSquare = not . isSumOfPrimeAndSquare

notSumNums = filter notSumOfPrimeAndSquare [1,3..]