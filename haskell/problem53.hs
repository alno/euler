fact 1 = 1
fact 2 = 2
fact r = r * fact (r-1)

factL i 1             = 1
factL i n | i == 0    = 1
          | otherwise = n * factL (i-1) (n-1)

comb n r = factL r n `div` fact r

combs = [ (n,r) | n <- [1..100], r <- [1..n], comb n r > 1000000 ]

combCount = length combs

main :: IO ()
main = print combCount
