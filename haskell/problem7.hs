isPrime []    x                  = True
isPrime (h:t) x | h*h > x        = True
                | x `mod` h == 0 = False
                | otherwise      = isPrime t x

primes = 2 : filter (isPrime primes) [3,5..]
