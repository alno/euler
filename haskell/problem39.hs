-- Brute force, need imporvements

import Data.Function
import Data.List

solutions p = [ (a,b,c) | a <- [1..p `div` 2], b <- [a..(p-a-1)], let c = p - a - b, a*a + b*b == c*c ]

numSolutions p = length (solutions p)

tuples = [ (x,numSolutions x) | x <- [1..1000] ]

max_tuple = maximumBy (compare `on` snd) tuples
