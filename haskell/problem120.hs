{-- 
  (a-1)^n + (a+1)^n = 
    2*n*a
--}

maxNum a = maximum [ (2*n*a) `mod` (a*a) | n <- [1..a `div` 2] ]

sumMaxNum = sum (map maxNum [3..1000])

main = print sumMaxNum
