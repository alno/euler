import Data.List

divisorSumFrom :: Int -> Int -> Int -> Int
divisorSumFrom i s x | i*i >  x       = s
                     | i*i == x       = s + i
                     | otherwise      = let (d,m) = divMod x i in
                                            if m == 0 then divisorSumFrom (i+1) (s+i+d) x
                                                      else divisorSumFrom (i+1)  s      x

divisorSum = divisorSumFrom 2 1

isAbundant x = x < divisorSum x

abundantNumbers = filter isAbundant [1..]

isAbundantSum x = any (\a -> isAbundant (x-a)) (takeWhile (<x) abundantNumbers)

notAbundantSumNumbers = filter (not.isAbundantSum) [1..28123]

sumNotAbundantSumNumbers = sum (map toInteger notAbundantSumNumbers)

abundantTuple x = case find (\a -> isAbundant (x-a)) (takeWhile (<x) abundantNumbers) of
                       Just a -> Just (x,a,x-a)
                       Nothing -> Nothing

abundantTuples = [ t | n <- [1..28123], let mt = abundantTuple n, mt /= Nothing, let Just t = mt ]