-- Brute-force solution

import Data.Char

fact 0 = 1
fact 1 = 1
fact 2 = 2
fact 3 = 6
fact 4 = 24
fact 5 = 120
fact 6 = 720
fact 7 = 5040
fact 8 = 40320
fact 9 = 362880

next n = sum $ map (fact.digitToInt) $ show n

seqLen' len s@(h:t) | elem n s = len
                    | otherwise = seqLen' (len+1) (n:s)
  where n = next h

seqLen n = seqLen' 1 [n]

seqLens = [ (n,seqLen n) | n <- [1..10^6] ]
count = length $ filter (\n -> seqLen n == 60) [1..10^6]

main = print count
