
noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime x = x /= 1 && noDivisors primes x

-- Diag numbers
diagNumbers n = [ start + sz, start + sz * 2, start + sz * 3, start + sz * 4 ] where sz = n*2; start = (sz-1)*(sz-1)

diagNumberList = [1]:map diagNumbers [1..]
diagPrimaryList = map ((filter isPrime).init) diagNumberList

countList list = scanl1 (+) (map length list)

diagCountList = countList diagNumberList

primePercentList = zipWith (\x y -> fromIntegral x / fromIntegral y) (countList diagPrimaryList) (countList diagNumberList)

sideAndPercent = zipWith (\x y -> (x,y)) [3,5..] (tail primePercentList)

firstTuple = head (dropWhile (\t -> snd t >= 0.1) sideAndPercent)

-- Main
main :: IO ()
main = print (fst firstTuple)
