noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Prime factors

nDivsFrom c x y | m == 0 = nDivsFrom (c+1) d y
                | otherwise = (x,c)
    where (d,m) = divMod x y

nDivs = nDivsFrom 0

primeFactorsExpApp factors (h:t) x | h > x              = factors
                                   | h * h > x          = ((x,1):factors)
                                   | otherwise          = primeFactorsExpApp ((h,c):factors) t d where (d,c) = nDivs x h

primeFactorsExp = primeFactorsExpApp [] primes

-- Number divisors

divisorsFact [] = [1]
divisorsFact ((p,q):t) = [ p^i * tdi | i <- [0..q], tdi <- td ] where td = divisorsFact t

divisors = divisorsFact . primeFactorsExp

-- Pentagonal

intSqrt :: Integer -> Integer
intSqrt n = truncate (sqrt (fromInteger n))

isPentagonal x = s*s == y && (s+1) `mod` 6 == 0 where y = 24 * x + 1
                                                      s = intSqrt y

pentagonal = [ n*(3*n-1) `div` 2 | n <- [0..] ]

-- Possible indices for n
inds d = [ fromInteger (n6 `div` 6) | k <- divisors d2, let nk = d2 `div` k, let n6 = nk + 1 - 3*k, n6 > 0, n6 `mod` 6 == 0 ] where d2 = 2*d

diffs = [ d | d <- pentagonal, n <- [ i*(3*i-1) `div` 2 | i <- [1..d `div` 3] ], isPentagonal (n+d) ]
diffs2 = [ d | d <- pentagonal, ni <- inds d, let n = pentagonal !! ni, isPentagonal (n+d), isPentagonal (2*n+d) ]

main :: IO ()
main = print (head diffs2)
