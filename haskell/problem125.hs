makePalindromes :: Integer -> [Integer]
makePalindromes n = [ read (s ++ rs), read ( s ++ tail rs) ] where s = show n; rs = reverse s

squareSums = scanl1 (+) [ n*n | n <- [0..]]

isPalindromic x = s == reverse s where s = show x

isSquareSumStart x y = sums !! 0 == x && sums !! 1 /= y where sums = dropWhile (<x) squareSums
{--
selectSquareSumsFrom i (h1:h2:t) n | h2 >= n = take (i*2) (h1:h2:t)
                                   | otherwise = selectSquareSumsFrom (i+1) (h2:t) n

selectSquareSums = selectSquareSumsFrom 0 squareSums

isSquareSum n = not (null [ x | x <- selectSquareSums n, isSquareSumStart (x-n) x ])
squareSum n = head [ (x,x-n) | x <- selectSquareSums n, isSquareSumStart (x-n) x ]

palindromes = concat (map makePalindromes [1..9999])
--}

selectSquareSumsFrom i (h1:h2:t) n | h2 - h1 > n = take i squareSums
                                   | otherwise   = selectSquareSumsFrom (i+1) (h2:t) n

selectSquareSums = selectSquareSumsFrom 1 squareSums

takeWhileNext cond (h1:h2:t) | cond h2   = h1:takeWhileNext cond (h2:t)
                             | otherwise = []

maxNum = 100000000
squareSumNums = [ n | a <- selectSquareSums maxNum, b <- takeWhileNext (<a) (dropWhile (<=(a-maxNum)) squareSums), let n = a-b, isPalindromic n ]

-- Main
main :: IO ()
main = print "Not completed"
