-- Check that there are no divisors of n in list
noDivisors l x = all (\h -> x `mod` h /= 0) $ takeWhile (\h -> h*h <= x) l

-- List of prime numbers
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime x = noDivisors primes x

-- Calculate all powers of number n
powers n = iterate (*n) 1

-- Merge two ascending lists
merge l1 [] = l1
merge [] l2 = l2
merge l1@(h1:t1) l2@(h2:t2) | h1 <  h2 = h1:merge t1 l2
                            | h1 >  h2 = h2:merge l1 t2
                            | h1 == h2 = h1:merge t1 t2

-- Merge productions
mergeProductions (m1:t1) l2 = head r1:merge (tail r1) (mergeProductions t1 l2) where r1 = map (*m1) l2

-- Calculate all productions of powers of multipliers mults in increasing order
productions []     = [1]
productions (m:mt) = mergeProductions (powers m) (productions mt)
