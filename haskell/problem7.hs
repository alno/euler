
noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : filter (noDivisors primes) [3,5..]

-- Function to check is the number is prime
isPrime = noDivisors primes

prime_10001 = primes !! 10000