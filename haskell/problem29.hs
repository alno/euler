import List

-- numbers = [ a^b | a <- [2..100], b <- [2..100] ]

merge [] l = l
merge l [] = l
merge l1@(h1:t1) l2@(h2:t2) | h1 < h2   = h1:merge t1 l2
                            | h2 < h1   = h2:merge l1 t2
                            | otherwise = h1:merge t1 t2

numbersFrom s 101 = s
numbersFrom s i   = numbersFrom (merge s (map (i^) [2..100])) (i+1)

numbers = numbersFrom [] 2

uniqCount = length numbers

main :: IO ()
main = print uniqCount
