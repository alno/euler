-- Brute-force solution, need imporvements

divisorSumFrom :: Int -> Int -> Int
divisorSumFrom i x | i*2 > x        = 0
                   | x `mod` i == 0 = i + divisorSumFrom (i+1) x
                   | otherwise      = divisorSumFrom (i+1) x

divisorSum = divisorSumFrom 1

amicable_numbers = [ (x,y) | x <- [1..10000], let y = divisorSum x, y < x, x == divisorSum y ]
sum_amicable_numbers = sum (map (\x -> fst x + snd x) amicable_numbers)