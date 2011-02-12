import Ratio
import Data.List
import Data.Function

next x = n - fromInteger (numerator n `div` denominator n) where n = x * 10
      
cycleLengthFrom d (h:t) | n == 0  = 0
                        | l <  d  = l + 1
                        | otherwise = cycleLengthFrom (d+1) (n:h:t)
  where n = next h
        l = length (takeWhile (n /=) (h:t))
        
cycleLength x = cycleLengthFrom 0 [x]

tuples = [ (n,cycleLength (1%n)) | n <- [2..1000] ]

maxTuple = maximumBy (compare `on` snd) tuples

main :: IO ()
main = print maxTuple