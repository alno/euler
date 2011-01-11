intSqrt :: Integer -> Integer
intSqrt n = truncate (sqrt (fromInteger n))

-- n(n+1)/2 = ((2*n+1)^2 - 1)/8

isTriangle x = s*s == y && (s-1) `mod` 2 == 0 where y = 8 * x + 1
                                                    s = intSqrt y

-- n(3n-1)/2 = ((6*n-1)^2 - 1)/24

isPentagonal x = s*s == y && (s+1) `mod` 6 == 0 where y = 24 * x + 1
                                                      s = intSqrt y

hexagonalNums = [ n*(2*n-1) | n <- [1..] ]

tripleNums = filter isPentagonal (filter isTriangle hexagonalNums)
