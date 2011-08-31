{--
  n = k^3
  n+p = m^3

  p = m^3 - k^3

  m = k+l
  p = (k+l)^3 - k^3 = 3*l*k^2 + 3*l^2*k + l^3 = l * (3*k^2 + 3*l*k + l^3)

  l = 1
  p = 3*k^2 + 3*k + 1

  d = 9 - 12(1-p) = 12p - 3
  k = -1+ds

--}


noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

intSqrt :: Int -> Int
intSqrt n = truncate (sqrt (fromIntegral n))

isSpecial p = ds * ds == d && ds `mod` 3 == 0 && ds > 3
  where d = 12*p-3
        ds = intSqrt d

specialCount = length $ filter isSpecial $ takeWhile (<10^6) primes

main = print specialCount
