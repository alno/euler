noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

isPrime x = x > 2 && noDivisors primes x

countWays :: Int -> Int -> Int

numWays = [1] : [ map (countWays n) [0..n] | n <- [1..] ]

getNumWays n m | m > n     = numWays !! n !! n
               | otherwise = numWays !! n !! m 

countWays n m = sum [getNumWays i p | p <- takeWhile (<=m) primes, let i = n - p ]
                                                                       
nWays = [ (n, numWays !! n !! n) | n <- [1..]]

main = print $ fst $ head $ dropWhile (\x -> snd x < 5000) nWays
