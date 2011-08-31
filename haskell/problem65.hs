import Ratio
import Data.Char

elist = 2:concat [ [1,2*k,1] | k <- [1..] ]

convergent' (h:t) 1 = h % 1
convergent' (h:t) n = h % 1 + 1 / convergent' t (n-1)

convergent n = convergent' elist n

main = print . sum . map digitToInt . show . numerator $ convergent 100
