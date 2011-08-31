-- (pn-1)^n + (pn+1)^n = 2*pn^n + ... + 2 * n*pn | n % 2 = 1
--                     = 2*pn^n + ... + 2        | n % 2 == 0

-- Brute-force solution

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

remainder n | n `mod` 2 == 1 = (2 * n * pn) `mod` (pn*pn)
            | otherwise      = 2
  where pn = primes !! fromInteger (n-1)

findFirstFrom n lim | remainder n > lim   = n
                    | otherwise           = findFirstFrom (n+1) $! lim

findFirst = findFirstFrom 1

main :: IO ()
main = print (findFirst (10^10))
