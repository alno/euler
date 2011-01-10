factProd p n | n == 1 = p
             | otherwise = factProd (p*n) (n-1)

fact = factProd 1

size = 20

pathCount n = fact (2*n) `div` fn `div` fn where fn = fact n

pathCount20 = pathCount 20