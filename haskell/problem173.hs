countTiles n k = n*n - k*k

tileCount = 10^6

intSqrt = ceiling . sqrt . fromIntegral

countOutlines n | n*n <= tileCount = (n - 1) `div` 2
                | otherwise        = (n - intSqrt (n*n-tileCount)) `div` 2

outlines = sum (map countOutlines [1..tileCount `div` 4+1])

main :: IO ()
main = print outlines
