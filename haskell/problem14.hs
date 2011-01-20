import Data.List
import Data.Function

seqLenFrom l n | n == 1    = l
               | m == 0    = seqLenFrom (l+1) d
               | otherwise = seqLenFrom (l+1) (3*n + 1)
    where (d,m) = divMod n 2

seqLen :: Int -> Int
seqLen = seqLenFrom 1

seqLens = map (\x -> (x,seqLen x)) [1..999999]

maxTuple = maximumBy (compare `on` snd) seqLens

-- Main
main :: IO ()
main = print "Not completed"
