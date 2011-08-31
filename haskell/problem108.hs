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

primeFactorsExpApp factors (h:t) x | h > x              = factors
                                   | h * h > x          = ((x,1):factors)
                                   | otherwise          = primeFactorsExpApp ((h,c):factors) t d where (d,c) = nDivs x h

primeFactorsExp = primeFactorsExpApp [] primes

solutionsFact []        = 1
solutionsFact ((p,q):t) = q*(2*c-1)+c where c = solutionsFact t

solutions = solutionsFact . primeFactorsExp

leastX = head (filter (\x -> solutions x > 1000) [1..])

main :: IO ()
main = print leastX
