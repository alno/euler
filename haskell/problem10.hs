-- Brute-force solution, need imporvements

isPrime []    x                  = True
isPrime (h:t) x | h*h > x        = True
                | x `mod` h == 0 = False
                | otherwise      = isPrime t x

sum_2000000 = sum (takeWhile (\x -> x < 2000000) primes)