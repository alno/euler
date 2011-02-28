-- Brute-force solution

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

divisorCountFor p (h:t) x | h > x     = p
                          | h * h > x = p * 2
                          | c > 0     = divisorCountFor (p*(c+1)) t $! d 
                          | otherwise = divisorCountFor p t $! d 
  where (d,c) = nDivs x h

divisorCount = divisorCountFor 1 primes

 -- Nums of divisors
                           
countPairsF a b i s | i >  10^7 = s
                    | a == b    = countPairsF b (divisorCount i) (i+1) $! (s+1)
                    | otherwise = countPairsF b (divisorCount i) (i+1) $! s

countPairs = countPairsF (divisorCount 1) (divisorCount 2) 3 0

main :: IO ()
main = print countPairs
