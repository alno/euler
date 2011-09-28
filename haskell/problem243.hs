import Data.List
import Data.Ratio

-- Check that there are no divisors of n in list
noDivisors l x = all (\h -> x `mod` h /= 0) $ takeWhile (\h -> h*h <= x) l

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime x = noDivisors primes x

-- For number n = П(pi^ki) where pi are different primes let P = П(1-1/pi) and M = {pi}
-- Then: 
--   coprimeCount n = n * P = phi
--   resilence n = phi / (n-1) = ((n-1) * P + P) / (n-1) = P + P/(n-1)
--   resilence n < limit => P < limit

limit = 15499 % 94744

nextPrime n = head $ dropWhile (<=n) primes
numbersWithRes = [ (n, r) | mults <- tail $ inits primes, 
                            let p = product $ map (\x -> 1 - 1 % x) mults,
                            p < limit, -- Optimization
                            prod <- [1..nextPrime $ last mults - 1], 
                            let n = prod * product mults,
                            let r = p + p / (n % 1 - 1), -- Resilence
                            r < limit ]
                            
main = print $ fst $ head numbersWithRes
