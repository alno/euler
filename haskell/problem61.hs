import Data.List

isConnected a b = a `mod` 100 == b `div` 100

maxNum = 9999
minNum = 1000

selectNums f = dropWhile (<minNum) $ takeWhile (<=maxNum) $ map f [1..]

triangleNumbers   = selectNums (\n -> n*(n+1) `div` 2 )
squareNumbers     = selectNums (\n -> n*n )
pentagonalNumbers = selectNums (\n -> n*(3*n-1) `div` 2 )
hexagonalNumbers  = selectNums (\n -> n*(2*n-1) )
heptagonalNumbers = selectNums (\n -> n*(5*n-3) `div` 2 )
octagonalNumbers  = selectNums (\n -> n*(3*n-2) )

numberCats = [ triangleNumbers, squareNumbers, pentagonalNumbers, hexagonalNumbers, heptagonalNumbers, octagonalNumbers ]

numbersExceptApp []    p e = []
numbersExceptApp (h:t) p e | elem h e  = numbersExceptApp t p e
                           | otherwise = [ (x,h) | x <- numberCats !! h, isConnected p x ] ++ numbersExceptApp t p e

numbersExcept = numbersExceptApp [0..length numberCats-1]

nums = [ [n1,n2,n3,n4,n5,n6] | let l1 = 5, n1 <- octagonalNumbers, (n2,l2) <- numbersExcept n1 [l1], (n3,l3) <- numbersExcept n2 [l1,l2], (n4,l4) <- numbersExcept n3 [l1,l2,l3], (n5,l5) <- numbersExcept n4 [l1,l2,l3,l4], (n6,l6) <- numbersExcept n5 [l1,l2,l3,l4,l5], isConnected n6 n1 ]

main :: IO ()
main = print (sum $ head nums)
