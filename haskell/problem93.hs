-- numbers digits ops
import Ratio
import Data.List
import Data.Function

ops a b | b == 0    = [ a + b, a - b, a * b ]
        | otherwise = [ a + b, a - b, a * b, a / b ]

merge2 [] l = l
merge2 l [] = l
merge2 l1@(h1:t1) l2@(h2:t2) | h1 < h2   = h1:merge2 t1 l2
                             | h1 > h2   = h2:merge2 l1 t2
                             | otherwise = h2:merge2 t1 t2

merge [l]   = l
merge (l:t) = merge2 l (merge t)

appFst h (a,b) = (h:a,b)
appSnd h (a,b) = (a,h:b)

split []    = [([],[])]
split (h:t) = map (appFst h) sl ++ map (appSnd h) sl where sl = split t

splitNE :: [Rational] -> [([Rational],[Rational])]
splitNE = filter (\t -> fst t /= [] && snd t /= []) . split

numbersDiv [x] = [x]
numbersDiv digits = merge [ (sort.nub) (ops n1 n2) | (d1,d2) <- splitNE digits, n1 <- numbersDiv d1, n2 <- numbersDiv d2 ]--nub (concat [op ])

numbers digits = numbersDiv (map (%1) digits)
posNumbers = dropWhile (<=0) . numbers

natNumbers digits = [ numerator n | n <- posNumbers digits, denominator n == 1 ]

seqLenFrom s []                = 0
seqLenFrom s (h:t) | h == s    = seqLenFrom (s+1) t
                   | otherwise = s-1

seqLen = seqLenFrom 1

lens = [ ((seqLen.natNumbers) l, l) | a <- [0..9], b <- [a+1..9], c <- [b+1..9], d <- [c+1..9], let l = [a,b,c,d] ]

maxTuple = maximumBy (compare `on` fst) lens

main :: IO ()
main = print maxTuple
