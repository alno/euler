
isIncreasing' prev []    = True
isIncreasing' prev (h:t) = h >= prev && isIncreasing' h t

isDecreasing' prev []    = True
isDecreasing' prev (h:t) = h <= prev && isDecreasing' h t

isBouncy' (h1:h2:t) | h1 > h2   = not $ isDecreasing' h2 t
                   | h1 < h2   = not $ isIncreasing' h2 t
                   | otherwise = isBouncy' (h2:t)
isBouncy' _ = False

isBouncy :: Integer -> Bool
isBouncy = isBouncy' . show

findBouncyRatio' b nb (h:t) ratio | nb * ratio == b  = nb + b
                                  | isBouncy h       = findBouncyRatio' (b+1) nb t ratio
                                  | otherwise        = findBouncyRatio' b (nb+1) t ratio
                              
findBouncyRatio = findBouncyRatio' 0 1 [2..]

main = print $ findBouncyRatio 99