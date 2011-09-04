-- Brute-force solution, needs improvements

numRects n m = sum [ (n-x+1)*(m-y+1) | x <- [1..n], y <- [1..m] ]

lim = 2*10^6

best t1@(a1,d1) t2@(a2,d2) | d2 < d1   = t2
                           | otherwise = t1

nearest' bv n m | r >= lim && m == 1 = bv 
                | r >= lim           = nearest' (best bv v) (n+1) 1
                | otherwise          = nearest' (best bv v) n (m+1)
  where r = numRects n m
        v = (n*m, abs (r - lim))

nearest = nearest' (0,lim) 1 1

main = print $ fst nearest
