s = 1000
triplets = [ (a,b,c) | a <- [1..s `div` 3], b <- [a..s-2*a], let c = s - a - b, a*a + b*b == c*c ]