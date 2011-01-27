countF n 1 = 1
countF n k | k >  n    = countF n n
           | k == n    = sum [ countF (n-i) i | i <- [1..n-1] ] + 1
           | otherwise = sum [ countF (n-i) i | i <- [1..k] ]
          
count n = countF n n - 1


memCountF a b = memCountI (a * (a-1) `div` 2 + b - 1)

memCountI = 
    let countF n 1 = 1
        countF n k | k >  n    = memCountF n n
                   | k == n    = sum [ memCountF (n-i) i | i <- [1..n-1] ] + 1
                   | otherwise = sum [ memCountF (n-i) i | i <- [1..k] ]
        
        countL n = map (countF n) [1..n]
    in (concat (map countL [1..]) !!)
    

main :: IO ()
main = print "Not completed"
