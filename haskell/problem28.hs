
diagNumbers n = [ start + sz, start + sz * 2, start + sz * 3, start + sz * 4 ] where sz = n*2; start = (sz-1)*(sz-1)

diagNumberList = [1]:map diagNumbers [1..]

sumList = scanl1 (+) (map sum diagNumberList)

sumList1001 = sumList !! 500

-- Main
main :: IO ()
main = print sumList1001
