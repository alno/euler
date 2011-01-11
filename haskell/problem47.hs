import Data.List

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime x = noDivisors primes x

divMulti x y | m == 0 = divMulti d y
             | otherwise = x
    where (d,m) = divMod x y

primeFactorsApp factors (h:t) x | length factors > 4 = Nothing
                                | h > x              = Just factors
                                | h * h >= x         = Just (x:factors)
                                | otherwise          = let (d,m) = divMod x h in
                                                            if  m == 0 then primeFactorsApp (h:factors) t (divMulti d h)
                                                                       else primeFactorsApp factors t x

primeFactors = primeFactorsApp [] primes

hasNPrimeFactorsApp fc (h:t) n x | fc > n             = False
                                 | h > x              = fc == n
                                 | h * h >= x         = fc + 1 == n
                                 | otherwise          = let (d,m) = divMod x h in
                                                            if  m == 0 then hasNPrimeFactorsApp (fc+1) t n (divMulti d h)
                                                                       else hasNPrimeFactorsApp fc t n x

hasNPrimeFactors = hasNPrimeFactorsApp 0 primes

nums = filter (hasNPrimeFactors 4) [4..]

firstFourFor (n1:t@(n2:n3:n4:_)) | n2 == n1 + 1 && n3 == n2 + 1 && n4 == n3 + 1 = n1
                                 | otherwise = firstFourFor t

firstFour = firstFourFor nums
